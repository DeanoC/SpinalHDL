package spinal.lib.system.dma.sg

import spinal.core._
import spinal.lib._

case class DmaMemoryLayout(bankCount : Int,
                           bankWords : Int,
                           bankWidth : Int,
                           priorityWidth : Int){
//  val ptrWidth = log2Up(size)
//  val ptrType = UInt(log2Up(size) bits)
//  val wordPerBank = bankWords*8/bankWidth/bankCount
}

case class DmaMemoryCoreParameter(layout : DmaMemoryLayout,
                                  writes : Seq[DmaMemoryCoreWriteParameter],
                                  reads: Seq[DmaMemoryCoreReadParameter])

case class DmaMemoryCoreReadParameter(bytes : Int, contextWidth : Int)
case class DmaMemoryCoreWriteParameter(bytes : Int, contextWidth : Int)

case class DmaMemoryCoreWriteCmd(layout : DmaMemoryLayout, p : DmaMemoryCoreWriteParameter) extends Bundle{
  val address = UInt(log2Up(layout.bankWords*layout.bankCount) bits)
  val data = Bits(p.bytes*8 bits)
  val mask = Bits(p.bytes bits)
  val priority = UInt(layout.priorityWidth bits)
  val context = Bits(p.contextWidth bits)
}
case class DmaMemoryCoreWriteRsp(layout : DmaMemoryLayout, p : DmaMemoryCoreWriteParameter) extends Bundle{
  val context = Bits(p.contextWidth bits)
}
case class DmaMemoryCoreReadCmd(layout : DmaMemoryLayout, p : DmaMemoryCoreReadParameter) extends Bundle{
  val address = UInt(log2Up(layout.bankWords*layout.bankCount)  bits)
  val priority = UInt(layout.priorityWidth bits)
  val context = Bits(p.contextWidth bits)
}
case class DmaMemoryCoreReadRsp(layout : DmaMemoryLayout, p : DmaMemoryCoreReadParameter) extends Bundle{
  val data = Bits(p.bytes*8 bits)
  val mask = Bits(p.bytes bits)
  val context = Bits(p.contextWidth bits)
}

case class DmaMemoryCoreWriteBus(layout : DmaMemoryLayout, p : DmaMemoryCoreWriteParameter) extends Bundle with IMasterSlave {
  val cmd = Stream(DmaMemoryCoreWriteCmd(layout, p))
  val rsp = Flow(DmaMemoryCoreWriteRsp(layout, p))

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }
}

case class DmaMemoryCoreReadBus(layout : DmaMemoryLayout, p : DmaMemoryCoreReadParameter) extends Bundle with IMasterSlave {
  val cmd = Stream(DmaMemoryCoreReadCmd(layout, p))
  val rsp = Stream(DmaMemoryCoreReadRsp(layout, p))

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }
}

case class DmaMemoryCore(p : DmaMemoryCoreParameter) extends Component{
  val io = new Bundle {
    val writes = Vec(p.writes.map(pp => slave(DmaMemoryCoreWriteBus(p.layout, pp))))
    val reads = Vec(p.reads.map(pp => slave(DmaMemoryCoreReadBus(p.layout, pp))))
  }
  case class BankWord() extends Bundle{
    val data = Bits(p.layout.bankWidth bits)
    val mask = Bits(p.layout.bankWidth/8 bits)
  }
  val banks = for(bankId <- 0 until p.layout.bankCount) yield new Area{
    val ram = Mem(BankWord(), p.layout.bankWords)
    val write = ram.writePort
    val read = ram.readSyncPort

    val writeOr = DataOr(write)
    write := writeOr.value

    val readOr = DataOr(read.cmd)
    read.cmd := readOr.value
  }



  val write = new Area{
    val ports = for(i <- 0 until p.writes.size) yield new Area{
      val priority = Reg(UInt(p.layout.priorityWidth << log2Up(p.writes.size) bits)) randBoot()
      def cmd = io.writes(i).cmd
      when(cmd.valid){
        priority := priority + cmd.priority
        when(cmd.ready) {
          priority := 0
        }
      }
    }

    case class Node() extends Bundle{
      val priority = Bool()
      val conflict = Bool()
    }
    val nodes = Vec(Vec(Node(), p.writes.size),  p.writes.size)

    for(self <- 0 until p.writes.size;
        other <- self + 1 until p.writes.size;
        if self != other){
      val bankMask = p.layout.bankCount - Math.max(p.writes(self).bytes, p.writes(other).bytes) *8/p.layout.bankWidth
      nodes(self)(other).priority := ports(self).priority > ports(other).priority
      nodes(self)(other).conflict := io.writes(self).cmd.valid && io.writes(other).cmd.valid && ((io.writes(self).cmd.address ^ io.writes(other).cmd.address) & bankMask) === 0
      nodes(other)(self).priority := !nodes(self)(other).priority
      nodes(other)(self).conflict :=  nodes(self)(other).conflict
    }
    val arbiter = for(self <- 0 until p.writes.size) yield new Area{
      val others = (0 until p.writes.size).filter(_ != self)
      val losedAgainst = B(others.map(other => nodes(self)(other)).map(node => node.conflict && !node.priority))
      val doIt = io.writes(self).cmd.valid && losedAgainst === 0
      for(bankId <- 0 until p.layout.bankCount){
        val port = banks(bankId).writeOr.newPort()
        val groupRange = log2Up(p.layout.bankCount)-1 downto log2Up(p.writes(self).bytes*8/p.layout.bankWidth)
        val sel = doIt && (io.writes(self).cmd.address(groupRange) ^ U(bankId))(groupRange) === 0
        when(sel){
          port.valid := True
          port.address   := io.writes(self).cmd.address >> log2Up(p.layout.bankCount)
          port.data.data := io.writes(self).cmd.data
          port.data.mask := io.writes(self).cmd.mask
        } otherwise {
          port := port.getZero
        }
      }
      ports(self).cmd.ready := doIt
      io.writes(self).rsp.valid := RegNext(doIt) init(False)
      io.writes(self).rsp.context := RegNext(io.writes(self).cmd.context)
    }
  }



  val read = new Area{
    val ports = for(i <- 0 until p.reads.size) yield new Area{
      val buffer = new Area{
        case class S0() extends Bundle{
          val context = Bits(p.reads(i).contextWidth bits)
          val address = UInt(widthOf(io.reads(i).cmd.address) bits)
        }
        val s0 = Flow(S0())
        val s1 = s0.stage()
        val bankPerGroup = p.reads(i).bytes*8/p.layout.bankWidth
        val groupSel = s1.address(log2Up(p.layout.bankCount)-1 downto log2Up(bankPerGroup))
        val bufferIn = Stream(DmaMemoryCoreReadRsp(p.layout, p.reads(i)))
        bufferIn.valid   := s1.valid
        bufferIn.context := s1.context
        bufferIn.data    := banks.grouped(bankPerGroup).map(g => Cat(g.map(_.read.rsp.data))).toSeq.read(groupSel)
        bufferIn.mask    := banks.grouped(bankPerGroup).map(g => Cat(g.map(_.read.rsp.mask))).toSeq.read(groupSel)
        val bufferOut = bufferIn.s2mPipe()
        io.reads(i).rsp << bufferOut

        val full = bufferOut.isStall
      }

      val priority = Reg(UInt(p.layout.priorityWidth << log2Up(p.reads.size) bits)) randBoot()
      val cmd = io.reads(i).cmd.haltWhen(buffer.full)
      when(cmd.valid){
        priority := priority + cmd.priority
        when(cmd.ready) {
          priority := 0
        }
      }
    }

    case class Node() extends Bundle{
      val priority = Bool()
      val conflict = Bool()
    }
    val nodes = Vec(Vec(Node(), p.reads.size),  p.reads.size)

    for(self <- 0 until p.reads.size;
        other <- self + 1 until p.reads.size){
      val bankMask = p.layout.bankCount - Math.max(p.reads(self).bytes, p.reads(other).bytes) *8/p.layout.bankWidth
      nodes(self)(other).priority := ports(self).priority > ports(other).priority
      nodes(self)(other).conflict := ports(self).cmd.valid && io.reads(other).cmd.valid && ((ports(self).cmd.address ^ io.reads(other).cmd.address) & bankMask) === 0
      nodes(other)(self).priority := !nodes(self)(other).priority
      nodes(other)(self).conflict :=  nodes(self)(other).conflict
    }
    val arbiter = for(self <- 0 until p.reads.size) yield new Area{
      val others = (0 until p.reads.size).filter(_ != self)
      val losedAgainst = B(others.map(other => nodes(self)(other)).map(node => node.conflict && !node.priority))
      val doIt = ports(self).cmd.valid && losedAgainst === 0
      for(bankId <- 0 until p.layout.bankCount){
        val port = banks(bankId).readOr.newPort()
        val groupRange = log2Up(p.layout.bankCount)-1 downto log2Up(p.reads(self).bytes*8/p.layout.bankWidth)
        val sel = doIt && (ports(self).cmd.address(groupRange) ^ U(bankId))(groupRange) === 0
        when(sel){
          port.valid   := True
          port.payload := ports(self).cmd.address >> log2Up(p.layout.bankCount)
        } otherwise {
          port := port.getZero
        }
      }
      ports(self).cmd.ready := doIt
      ports(self).buffer.s0.valid := doIt
      ports(self).buffer.s0.context := ports(self).cmd.context
      ports(self).buffer.s0.address := ports(self).cmd.address
    }
  }
}