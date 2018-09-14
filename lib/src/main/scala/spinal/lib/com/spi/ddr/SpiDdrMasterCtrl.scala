package spinal.lib.com.spi.ddr


import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.com.spi.SpiKind
import spinal.lib.fsm.{State, StateMachine}

import scala.collection.mutable.ArrayBuffer

case class DdrPin() extends Bundle with IMasterSlave{
  val writeEnable = Bool
  val read,write = Bits(2 bits)

  override def asMaster(): Unit = {
    out(write,writeEnable)
    in(read)
  }
}


case class SpiDdrParameter(dataWidth : Int = 2,
                           ssWidth : Int = 1)

case class SpiDdrMaster(p : SpiDdrParameter) extends Bundle with IMasterSlave{
  import p._

  val sclk = DdrPin()
  val data = Vec(DdrPin(), dataWidth)
  val ss   = if(ssWidth != 0) Bits(ssWidth bits) else null

  override def asMaster(): Unit = {
    master(sclk)
    if(ssWidth != 0) out(ss)
    data.foreach(master(_))
  }
}


object SpiDdrMasterCtrl {
  def apply(p : Parameters) = new TopLevel(p)



  def main(args: Array[String]): Unit = {
    SpinalVerilog(new TopLevel(Parameters(8,12,SpiDdrParameter(dataWidth = 4,ssWidth = 3)).addAllMods()))
  }




  //  case class ParameterMapping(position : Int, phase : Int)
  case class Mod(id : Int, writeMapping : Seq[Int], readMapping : Seq[Int]){
    assert(writeMapping.length == readMapping.length)
    def bitrate = readMapping.length
  }
  case class Parameters(dataWidth : Int,
                        timerWidth : Int,
                        spi : SpiDdrParameter,
                        mods : ArrayBuffer[Mod] = ArrayBuffer()){
    def ssGen = spi.ssWidth != 0
    def addFullDuplex(id : Int): this.type ={
      mods += Mod(id, List(0), List(1))
      this
    }
    def addHalfDuplex(id : Int, spiWidth : Int, ddr : Boolean): this.type = {
      val low = 0 until spiWidth
      val top = spi.dataWidth until spi.dataWidth + spiWidth
      if(ddr)
        mods += Mod(id, top ++ low, top ++ low)
      else
        mods += Mod(id, low, low)
      this
    }
    def addAllMods(): this.type ={
      if(dataWidth >= 2) addFullDuplex(0)
      for((spiWidth, o) <- (2 to spi.dataWidth).filter(isPow2(_)).zipWithIndex){
        addHalfDuplex(2+o*2, spiWidth, false)
        if(spiWidth*2 <= dataWidth) addHalfDuplex(2+o*2 + 1, spiWidth, true)
      }
      this
    }
  }

  case class Config(p: Parameters) extends Bundle {
    val kind = SpiKind()
    val sclkToogle = UInt(p.timerWidth bits)
    val fullRate = Bool
    val mod = in UInt(log2Up(p.mods.map(_.id).max + 1) bits)

    val ss = ifGen(p.ssGen) (new Bundle {
      val activeHigh = Bits(p.spi.ssWidth bits)
      val setup = UInt(p.timerWidth bits)
      val hold = UInt(p.timerWidth bits)
      val disable = UInt(p.timerWidth bits)
    })
  }

  case class Cmd(p: Parameters) extends Bundle{
    val kind = Bool
    val read, write = Bool
    val data = Bits(p.dataWidth bits)

    def isData = !kind
    def isSs = kind
    def getSsEnable = data.msb
    def getSsId = U(data(0, log2Up(p.spi.ssWidth) bits))
  }

  case class Rsp(p: Parameters) extends Bundle{
    val data = Bits(p.dataWidth bits)
  }


  case class MemoryMappingParameters(ctrl : Parameters,
                                     cmdFifoDepth : Int = 32,
                                     rspFifoDepth : Int = 32,
                                     xip : XipBusParameters = null)

  case class XipBusParameters(addressWidth : Int, dataWidth : Int)
  case class XipBus(p : XipBusParameters) extends Bundle with IMasterSlave{
    val cmd = Stream(UInt(p.addressWidth bits))
    val rsp = Flow(Bits(p.dataWidth bits))

    override def asMaster(): Unit = {
      master(cmd)
      slave(rsp)
    }
  }

  class TopLevel(p: Parameters) extends Component {
    setDefinitionName("SpiDdrMasterCtrl")

    val io = new Bundle {
      val config = in(Config(p))
      val cmd = slave(Stream(Cmd(p)))
      val rsp = master(Flow(Rsp(p)))
      val spi = master(master(SpiDdrMaster(p.spi)))


      def driveFrom(bus : BusSlaveFactory, baseAddress : Int = 0)(mapping : MemoryMappingParameters) = new Area {
        import mapping._
        require(cmdFifoDepth >= 1)
        require(rspFifoDepth >= 1)

        require(cmdFifoDepth < 32.kB)
        require(rspFifoDepth < 32.kB)

        //CMD
        val cmdLogic = new Area {
          val streamUnbuffered = Stream(Cmd(p))
          streamUnbuffered.valid := bus.isWriting(address = baseAddress + 0)
          bus.nonStopWrite(streamUnbuffered.data, bitOffset = 0)
          bus.nonStopWrite(streamUnbuffered.write, bitOffset = 8)
          bus.nonStopWrite(streamUnbuffered.read, bitOffset = 9)
          bus.nonStopWrite(streamUnbuffered.kind, bitOffset = 11)


          bus.createAndDriveFlow(Cmd(p),address = baseAddress + 0).toStream
          val (stream, fifoAvailability) = streamUnbuffered.queueWithAvailability(cmdFifoDepth)
          cmd << stream
          bus.read(fifoAvailability, address = baseAddress + 4, 16)
        }

        //RSP
        val rspLogic = new Area {
          val (stream, fifoOccupancy) = rsp.queueWithOccupancy(rspFifoDepth)
          bus.readStreamNonBlocking(stream, address = baseAddress + 0, validBitOffset = 31, payloadBitOffset = 0)
          bus.read(fifoOccupancy, address = baseAddress + 0, 16)
        }

        //Status
        val interruptCtrl = new Area {
          val cmdIntEnable = bus.createReadAndWrite(Bool, address = baseAddress + 4, 0) init(False)
          val rspIntEnable  = bus.createReadAndWrite(Bool, address = baseAddress + 4, 1) init(False)
          val cmdInt = bus.read(cmdIntEnable & !cmdLogic.stream.valid, address = baseAddress + 4, 8)
          val rspInt = bus.read(rspIntEnable &  rspLogic.stream.valid, address = baseAddress + 4, 9)
          val interrupt = rspInt || cmdInt
        }

        //Configs
        bus.drive(config.kind, baseAddress + 8, bitOffset = 0)
        bus.drive(config.mod, baseAddress + 8, bitOffset = 4)
        bus.drive(config.sclkToogle, baseAddress + 0x20)
        bus.drive(config.fullRate  , baseAddress + 0x20, bitOffset = 31)
        bus.drive(config.ss.setup,   baseAddress + 0x24)
        bus.drive(config.ss.hold,    baseAddress + 0x28)
        bus.drive(config.ss.disable, baseAddress + 0x2C)



        val xip = ifGen(mapping.xip != null) (new Area{
          val xipBus = XipBus(mapping.xip)
          val enable = RegInit(False)
          val instructionEnable = Reg(Bool)
          val instructionData = Reg(Bits(8 bits))
          val dummyCount = Reg(UInt(4 bits))
          val dummyData = Reg(Bits(8 bits))


          val fsm = new StateMachine{
            val doLoad, doPayload, done = False
            val loadedValid = RegInit(False)
            val loadedAddress = Reg(UInt(32 bits))
            val hit = loadedValid && loadedAddress + mapping.xip.dataWidth/8 === xipBus.cmd.payload

            val IDLE, INSTRUCTION, ADDRESS, DUMMY, PAYLOAD = State()
            setEntry(IDLE)

            cmd.valid := False
            IDLE.whenIsActive{
              when(doLoad){
                cmd.valid := True
                cmd.kind := True
                cmd.data := 1 << cmd.data.high
                when(cmd.ready) {
                  loadedAddress := xipBus.cmd.payload
                  when(instructionEnable) {
                    goto(INSTRUCTION)
                  } otherwise {
                    goto(ADDRESS)
                  }
                }
              }
            }

            INSTRUCTION.whenIsActive{
              cmd.valid := True
              cmd.kind := False
              cmd.write := True
              cmd.read := False
              cmd.data := instructionData
              when(cmd.ready) {
                goto(ADDRESS)
              }
            }

            val counter = Reg(UInt(4 bits)) init(0)
            ADDRESS.onEntry(counter := 0)
            ADDRESS.whenIsActive{
              cmd.valid := True
              cmd.kind := False
              cmd.write := True
              cmd.read := False
              cmd.data := loadedAddress.subdivideIn(8 bits).reverse(counter(1 downto 0)).asBits
              when(cmd.ready) {
                counter := counter + 1
                when(counter === 3) {
                  goto(DUMMY)
                }
              }
            }

            DUMMY.onEntry(counter := 0)
            DUMMY.whenIsActive{
              cmd.valid := True
              cmd.kind := False
              cmd.write := True
              cmd.read := False
              cmd.data := dummyData
              when(cmd.ready) {
                counter := counter + 1
                when(counter === dummyCount) {
                  loadedValid := True
                  goto(PAYLOAD)
                }
              }
            }

            PAYLOAD.onEntry(counter := 0)
            PAYLOAD.whenIsActive{
              when(doPayload) {
                cmd.valid := True
                cmd.kind := False
                cmd.write := False
                cmd.read := True
                when(counter === mapping.xip.dataWidth/8-1){
                  done := True
                  loadedAddress := loadedAddress + mapping.xip.dataWidth/8
                }
              }

              when(doLoad){
                cmd.valid := True
                cmd.kind := True
                cmd.data := 0
                when(cmd.ready) {
                  loadedValid := False
                  goto(IDLE)
                }
              }
            }
          }


          xipBus.cmd.ready := False
          when(enable){
            when(xipBus.cmd.valid){
              when(fsm.hit){
                fsm.doPayload := True
                xipBus.cmd.ready := fsm.done
              } otherwise {
                fsm.doLoad := True
              }
            }
          }

          val rspCounter = Counter(mapping.xip.dataWidth/8)
          val rspBuffer = Reg(Bits(mapping.xip.dataWidth-8 bits))
          when(enable && rsp.valid){
            rspCounter.increment()
            rspBuffer := rsp.payload ## (rspBuffer >> 8)
          }

          xipBus.rsp.valid := rspCounter.willOverflow
          xipBus.rsp.payload := rsp.payload ## rspBuffer
        })
      }
    }

    val timer = new Area{
      val counter = Reg(UInt(p.timerWidth bits))
      val reset = False
      val ss = ifGen(p.ssGen) (new Area{
        val setupHit    = counter === io.config.ss.setup
        val holdHit     = counter === io.config.ss.hold
        val disableHit  = counter === io.config.ss.disable
      })
      val sclkToogleHit = counter === io.config.sclkToogle

      counter := counter + 1
      when(reset){
        counter := 0
      }
    }



    val widths = p.mods.map(m => m.bitrate).distinct.sorted







    val fsm = new Area {
      val state = RegInit(False)
      val counter = Reg(UInt(log2Up(p.dataWidth) bits)) init(0)
      val counterPlus = counter +  io.config.mod.muxList(U(0), p.mods.map(m => m.id -> U(m.bitrate))).resized
      val readFill, readDone = False
      val ss = RegInit(B((1 << p.spi.ssWidth) - 1, p.spi.ssWidth bits))
      io.spi.ss := ss

      io.cmd.ready := False
      when(io.cmd.valid) {
        when(io.cmd.isData) {
          timer.reset := timer.sclkToogleHit
          when(timer.sclkToogleHit){
            state := !state
          }
          when((timer.sclkToogleHit && state) || io.config.fullRate) {
            counter := counterPlus
            readFill := True
            when(counterPlus === 0){
              io.cmd.ready := True
              readDone := io.cmd.read
            }
          }
        } otherwise {
          if (p.ssGen) {
            when(io.cmd.getSsEnable) {
              ss(io.cmd.getSsId) := False
              when(timer.ss.setupHit) {
                io.cmd.ready := True
              }
            } otherwise {
              when(!state) {
                when(timer.ss.holdHit) {
                  state := True
                  timer.reset := True
                }
              } otherwise {
                ss(io.cmd.getSsId) := True
                when(timer.ss.disableHit) {
                  io.cmd.ready := True
                }
              }
            }
          }
        }
      }

      //Idle states
      when(!io.cmd.valid || io.cmd.ready){
        state := False
        counter := 0
        timer.reset := True
      }
    }


    val maxBitRate = p.mods.map(m => m.bitrate).max
    val outputPhy = new Area {

      val sclkWrite = Bits(2 bits)
      sclkWrite := 0
      when(io.cmd.valid && io.cmd.isData){
        when(io.config.fullRate){
          sclkWrite := !io.config.kind.cpha ## io.config.kind.cpha
        } otherwise {
          sclkWrite := (default -> (fsm.state ^ io.config.kind.cpha))
        }
      }


      io.spi.sclk.writeEnable := True
      io.spi.sclk.write := sclkWrite ^ B(sclkWrite.range -> io.config.kind.cpol)




      val dataWrite = Bits(maxBitRate bits)
      val widthSel = io.config.mod.muxList(U(0), p.mods.map(m => m.id -> U(widths.indexOf(m.bitrate))))
      dataWrite.assignDontCare()
      switch(widthSel){
        for((width, widthId) <- widths.zipWithIndex){
          is(widthId){
            dataWrite(0, width bits) := io.cmd.data.subdivideIn(width bits).reverse(fsm.counter >> log2Up(width))
          }
        }
      }


      io.spi.data.foreach(_.writeEnable := False)
      io.spi.data.foreach(_.write.assignDontCare())

      switch(io.config.mod){
        for(mod <- p.mods){
          val modIsDdr = mod.writeMapping.exists(_ >= p.spi.dataWidth)
          is(mod.id) {
            when(io.cmd.valid && io.cmd.write){
              mod.writeMapping.map(_ % p.spi.dataWidth).distinct.foreach(i => io.spi.data(i).writeEnable := True)
            }

            when(io.config.fullRate){
              for((targetId, sourceId) <- mod.writeMapping.zipWithIndex){
                io.spi.data(targetId % p.spi.dataWidth).write(targetId / p.spi.dataWidth) := dataWrite(sourceId)
                if(!modIsDdr) io.spi.data(targetId % p.spi.dataWidth).write(1-targetId / p.spi.dataWidth) := dataWrite(sourceId)
              }
            } otherwise {
              if(modIsDdr) {
                when(!fsm.state) {
                  for ((targetId, sourceId) <- mod.writeMapping.zipWithIndex if targetId < p.spi.dataWidth) {
                    io.spi.data(targetId % p.spi.dataWidth).write := (default -> dataWrite(sourceId))
                  }
                } otherwise {
                  for ((targetId, sourceId) <- mod.writeMapping.zipWithIndex if targetId >= p.spi.dataWidth) {
                    io.spi.data(targetId % p.spi.dataWidth).write := (default -> dataWrite(sourceId))
                  }
                }
              } else {
                for ((targetId, sourceId) <- mod.writeMapping.zipWithIndex) {
                  io.spi.data(targetId % p.spi.dataWidth).write := (default -> dataWrite(sourceId))
                }
              }
            }
          }
        }
      }
    }


    val inputPhy = new Area{
      def sync[T <: Data](that : T, init : T = null) = Delay(that,2,init=init)
      val mod = sync(io.config.mod)
      val fullRate = sync(io.config.fullRate)
      val readFill = sync(fsm.readFill, False)
      val readDone = sync(fsm.readDone, False)
      val buffer = Reg(Bits(p.dataWidth - p.mods.map(_.bitrate).min bits))
      val bufferNext = Bits(p.dataWidth bits).assignDontCare().allowOverride
      val widthSel = mod.muxList(U(0),p.mods.map(m => m.id -> U(widths.indexOf(m.bitrate))))
      val dataWrite, dataRead = Bits(maxBitRate bits)
      val dataReadBuffer = RegNextWhen(Cat(io.spi.data.map(_.read(1))), !sync(fsm.state))
      val dataReadSource = Cat(io.spi.data.map(_.read(0))) ## dataReadBuffer

      dataRead.assignDontCare()

      switch(mod){
        for(mod <- p.mods){
          //        val modIsDdr = mod.writeMapping.exists(_ >= p.spi.dataWidth)
          is(mod.id) {
            when(fullRate){
              for((sourceId, targetId) <- mod.readMapping.zipWithIndex) {
                dataRead(targetId) := io.spi.data(sourceId % p.spi.dataWidth).read(1 - sourceId / p.spi.dataWidth)
              }
            } otherwise {
              for((sourceId, targetId) <- mod.readMapping.zipWithIndex) {
                dataRead(targetId) := dataReadSource(sourceId)
              }
            }
          }
        }
      }


      switch(widthSel) {
        for ((width,widthId) <- widths.zipWithIndex) {
          is(widthId) {
            bufferNext := (buffer ## dataRead(0, width bits)).resized
            when(readFill) { buffer := bufferNext.resized }
          }
        }
      }

      io.rsp.valid := readDone
      io.rsp.data := bufferNext
    }
  }
}
