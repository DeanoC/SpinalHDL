package spinal.lib.blackbox.xilinx.s7

import spinal.core._

case class S7_BRAM(
  val BRAMSize : Int = 18,        // 18 or 36 Kilo bits
  val DOA_REG : Boolean = false,
  val DOB_REG : Boolean = false,
  val RAM_EXTENSION_A : String = "NONE",
  val RAM_EXTENSION_B : String = "NONE",
  val READ_WIDTH_A : Int = 0,  // TDP mode = 0/1/2/4/9/18/36
  val WRITE_WIDTH_A : Int = 0, // SDP Read/Write = 64/72 other 0/1/2/4/9/18/36/72
  val READ_WIDTH_B : Int = 0,  // ECC 0/64 bit
  val WRITE_WIDTH_B : Int = 0,
  val WRITE_MODE_A : String = "NO_CHANGE",
  val WRITE_MODE_B : String = "NO_CHANGE",
  val RDADDR_COLLISION_HWCONFIG : String = "DELAYED_WRITE", // PERFORMANCE if not READ_FIRST
  val RAM_MODE : String = "TDP"
) extends BlackBox
{
  noIoPrefix()
  setDefinitionName(s"RAMB${BRAMSize}E1")
  addGeneric("DOA_REG", DOA_REG)
  addGeneric("DOB_REG", DOB_REG)
  if(BRAMSize == 36)
  {
    addGeneric("RAM_EXTENSION_A", RAM_EXTENSION_A)
    addGeneric("RAM_EXTENSION_B", RAM_EXTENSION_B)
  }
  addGeneric("READ_WIDTH_A", READ_WIDTH_A)
  addGeneric("READ_WIDTH_B", READ_WIDTH_B)
  addGeneric("WRITE_WIDTH_A", WRITE_WIDTH_A)
  addGeneric("WRITE_WIDTH_B", WRITE_WIDTH_B)
  addGeneric("WRITE_MODE_A", WRITE_MODE_A)
  addGeneric("WRITE_MODE_B", WRITE_MODE_B)
  if(WRITE_MODE_A != "READ_FIRST" && WRITE_MODE_B != "READ_FIRST")
  {
    // if both ports are not READ_FIRST we can safely set PERFORMANCE mode
    addGeneric("RDADDR_COLLISION_HWCONFIG", "PERFORMANCE")
  } else
  {
    addGeneric("RDADDR_COLLISION_HWCONFIG", RDADDR_COLLISION_HWCONFIG)
  }
  addGeneric("RAM_MODE", RAM_MODE)

  if( READ_WIDTH_A == 0 && READ_WIDTH_B == 0)
  {
    println("DESIGN WARNING - S7_BRAM with both read ports set to 0 width will be discarded")
  }

  if( WRITE_WIDTH_A == 0 && WRITE_WIDTH_B == 0)
  {
    println("DESIGN WARNING - S7_BRAM with both write ports set to 0 width will be discarded")
  }

  val io = new Bundle {
    var DIADI : Bits = in Bits((if(BRAMSize == 18) 16 else 32) bits)
    var DIPADIP : Bits = in Bits((if(BRAMSize == 18) 2 else 4) bits)
    var DIBDI : Bits = in Bits((if(BRAMSize == 18) 16 else 32) bits)
    var DIPBDIP : Bits = in Bits((if(BRAMSize == 18) 2 else 4) bits)

    var ADDRARDADDR = in Bits((if(BRAMSize == 18) 14 else 16) bits)
    var ADDRBWRADDR = in Bits((if(BRAMSize == 18) 14 else 16) bits)
    var WEA : Bits = in Bits((if(BRAMSize == 18) 2 else 4) bits)
    var WEBWE : Bits = in Bits((if(BRAMSize == 18) 4 else 8) bits)

    val ENARDEN = in Bool
    val ENBWREN = in Bool

    val RSTREGARSTREG = in Bool
    val RSTREGB = in Bool
    val RSTRAMARSTRAM = in Bool
    val RSTRAMB = in Bool

    val CLKARDCLK = in Bool
    val CLKBWRCLK = in Bool

    val REGCEAREGCE = in Bool
    val REGCEB = in Bool

    val CASCADEINA : Bool = if(BRAMSize == 36) in Bool() else null
    val CASCADEINB : Bool = if(BRAMSize == 36) in Bool() else null
    val CASCADEOUTA : Bool = if(BRAMSize == 36) out Bool() else null
    val CASCADEOUTB : Bool = if(BRAMSize == 36) out Bool() else null

    var DOADO : Bits = out Bits((if(BRAMSize == 18) 16 else 32) bits)
    var DOPADOP : Bits = out Bits((if(BRAMSize == 18) 2 else 4) bits)
    var DOBDO : Bits = out Bits((if(BRAMSize == 18) 16 else 32) bits)
    var DOPBDOP : Bits = out Bits((if(BRAMSize == 18) 2 else 4) bits)
  }
}

case class SDP_BRAM(
  val BRAMSize : Int = 18,   // 18 or 36 Kilo bits
  val readWidth : Int = 36,  // Read or Write = 32/36 or 64/72 
  val writeWidth : Int = 36, // other 1/2/4/9/18/36 (+72 when BRAMSize = 36)
  val outputRegister : Boolean = false,
  val collisionMode : String = "NO_CHANGE"
  ) extends Component
{
  def power2er(x : Int) : Int = x match {
    case 18 => 16
    case 36 => 32
    case 72 => 64
    case default => default
  }

  val pow2ReadWidth = power2er(readWidth)  
  val pow2WriteWidth = power2er(writeWidth)

  val readDepth = if(BRAMSize == 18) ((16 * 1 KiB) / pow2ReadWidth) else
                                     ((32 * 1 KiB) / pow2ReadWidth)
  val readAddressLines = log2Up(readDepth)            
  val writeDepth = if(BRAMSize == 18) ((16 * 1 KiB) / pow2WriteWidth) else
                                     ((32 * 1 KiB) / pow2WriteWidth)
  val writeAddressLines = log2Up(writeDepth)            
  var isReadPrimaryPort : Boolean = if(BRAMSize == 18) 
                                      (readWidth == 32) || (readWidth == 36)
                                    else 
                                      (readWidth == 64) || (readWidth == 72)
  var isWritePrimaryPort : Boolean = if(BRAMSize == 18) 
                                      (writeWidth == 32) || (writeWidth == 36)
                                    else 
                                      (writeWidth == 64) || (writeWidth == 72)
    
  var io = new Bundle{
    val readAddress = in Bits(readAddressLines bits)
    val readData = out Bits(readWidth bits) 
    val writeAddress = in Bits(writeAddressLines bits)
    val writeData = in Bits(writeWidth bits) 

    val writeByteEnable = in Bits(pow2WriteWidth / 8 bits)
    val readEnable = in Bool
    val writeEnable = in Bool
    val resetReg = in Bool
    val resetRAM = in Bool
    val readClock = in Bool
    val writeClock = in Bool
    val regCE = in Bool
  }

  // do we need to split the data over the 2 ports?
  var readWidthA = readWidth
  var readWidthB = 0
  var writeWidthA = writeWidth
  var writeWidthB = 0

  if(readWidth > BRAMSize) {
    readWidthA = readWidth / 2
    readWidthB = readWidth / 2
  }
  if(writeWidth > BRAMSize) {
    writeWidthA = writeWidth / 2
    writeWidthB = writeWidth / 2
  }

  val bram = S7_BRAM( BRAMSize = BRAMSize,
                      RAM_MODE = "SDP", 
                      READ_WIDTH_A = readWidthA,
                      READ_WIDTH_B = readWidthB,
                      WRITE_WIDTH_A = writeWidthA,
                      WRITE_WIDTH_B = writeWidthB,
                      WRITE_MODE_A = collisionMode,
                      WRITE_MODE_B = collisionMode,
                      DOA_REG = outputRegister,
                      DOB_REG = outputRegister )

  val maxAddressLen = if(BRAMSize == 18) 14 else 16

  bram.io.ADDRARDADDR := Cat(B"16'hFFFF".resize(maxAddressLen - readAddressLines), io.readAddress)
  bram.io.ADDRBWRADDR := Cat(B"16'hFFFF".resize(maxAddressLen - writeAddressLines), io.writeAddress)
  bram.io.WEA := 0
  bram.io.WEBWE := io.writeByteEnable.resize( if(BRAMSize == 18) 4 else 8 )
  bram.io.ENARDEN := io.readEnable
  bram.io.ENBWREN := io.writeEnable
  bram.io.RSTREGARSTREG := io.resetReg
  bram.io.RSTREGB := io.resetReg
  bram.io.RSTRAMARSTRAM := io.resetRAM
  bram.io.RSTRAMB := io.resetRAM
  bram.io.CLKARDCLK := io.readClock
  bram.io.CLKBWRCLK := io.writeClock
  bram.io.REGCEAREGCE := io.regCE
  bram.io.REGCEB := io.regCE

  if(BRAMSize == 36)
  {
    bram.io.CASCADEINA := False
    bram.io.CASCADEINB := False
  }

  readWidth match {
    case rw if(rw < 18) => {
      assert(!isReadPrimaryPort)
      assert( rw == 1 || rw == 2 || rw == 4 || rw == 9)
      bram.io.DOADO := 0
      bram.io.DOBDO <> io.readData(0 until rw)
      bram.io.DOPADOP := 0
      bram.io.DOPBDOP := 0
    }
    case 18 => {
      assert(!isReadPrimaryPort)
      if(BRAMSize == 18) {
        bram.io.DOADO := 0
        bram.io.DOBDO <> io.readData(0 until 16)
        bram.io.DOPADOP := 0
        bram.io.DOPBDOP <> io.readData(16 until 18)
      } else
      {
        bram.io.DOADO := 0
        bram.io.DOBDO <> io.readData(0 until 18)
        bram.io.DOPADOP := 0
        bram.io.DOPBDOP := 0
      }
    }
    case 32 if(isReadPrimaryPort) => {
      assert(BRAMSize == 18)
      bram.io.DOADO <> io.readData(0 until 16)
      bram.io.DOBDO <> io.readData(16 until 32)
      bram.io.DOPADOP := 0
      bram.io.DOPBDOP := 0
    }
    case 36 => {
      if(BRAMSize == 18) {
        bram.io.DOADO <> io.readData(0 until 16)
        bram.io.DOBDO <> io.readData(16 until 32)
        bram.io.DOPADOP <> io.readData(32 until 34)
        bram.io.DOPBDOP <> io.readData(34 until 36)
      } else
      {
        bram.io.DOADO := 0
        bram.io.DOBDO <> io.readData(0 until 32)
        bram.io.DOPADOP := 0
        bram.io.DOPBDOP <> io.readData(34 until 36)
      }
    }
    case 64 if(isReadPrimaryPort) => {
      assert(BRAMSize == 36)
      bram.io.DOADO <> io.readData(0 until 32)
      bram.io.DOBDO <> io.readData(32 until 64)
      bram.io.DOPADOP := 0
      bram.io.DOPBDOP := 0
    }
    case 72 => {
      assert(BRAMSize == 36)
      bram.io.DOADO <> io.readData(0 until 32)
      bram.io.DOBDO <> io.readData(32 until 64)
      bram.io.DOPADOP <> io.readData(64 until 68)
      bram.io.DOPBDOP <> io.readData(68 until 72)
    }
  }

  writeWidth match {
    case ww if(ww < 18) => {
      assert(!isWritePrimaryPort)
      assert( ww == 1 || ww == 2 || ww == 4 || ww == 9)
      bram.io.DIADI := 0
      bram.io.DIBDI <> io.writeData(0 until ww)
      bram.io.DIPADIP := 0
      bram.io.DIPBDIP := 0
    }
    case 18 => {
      assert(!isWritePrimaryPort)
      if(BRAMSize == 18) {
        bram.io.DIADI := 0
        bram.io.DIBDI <> io.writeData(0 until 16)
        bram.io.DIPADIP := 0
        bram.io.DIPBDIP <> io.writeData(16 until 18)
      } else
      {
        bram.io.DIADI := 0
        bram.io.DIBDI <> io.writeData(0 until 18)
        bram.io.DIPADIP := 0
        bram.io.DIPBDIP := 0
      }
    }
    case 32 if(isWritePrimaryPort) => {
      assert(BRAMSize == 18)
      bram.io.DIADI <> io.writeData(0 until 16)
      bram.io.DIBDI <> io.writeData(16 until 32)
      bram.io.DIPADIP := 0
      bram.io.DIPBDIP := 0
    }
    case 36 => {
      if(BRAMSize == 18) {
        bram.io.DIADI <> io.writeData(0 until 16)
        bram.io.DIBDI <> io.writeData(16 until 32)
        bram.io.DIPADIP <> io.writeData(32 until 34)
        bram.io.DIPBDIP <> io.writeData(34 until 36)
      } else
      {
        bram.io.DIADI := 0
        bram.io.DIBDI <> io.writeData(0 until 32)
        bram.io.DIPADIP := 0
        bram.io.DIPBDIP <> io.writeData(34 until 36)
      }
    }
    case 64 if(isWritePrimaryPort) => {
      assert(BRAMSize == 36)
      bram.io.DIADI <> io.writeData(0 until 32)
      bram.io.DIBDI <> io.writeData(32 until 64)
      bram.io.DIPADIP := 0
      bram.io.DIPBDIP := 0
    }
    case 72 => {
      assert(BRAMSize == 36)
      bram.io.DIADI <> io.writeData(0 until 32)
      bram.io.DIBDI <> io.writeData(32 until 64)
      bram.io.DIPADIP <> io.writeData(64 until 68)
      bram.io.DIPBDIP <> io.writeData(68 until 72)
    }
  }
}

case class S7_FIFO(
    val BRAMSize : Int = 18,
    val DATA_WIDTH : Int = 4,  // 4, 9, 18, 36, 72 
    val ALMOST_FULL_OFFSET : Int = 5,
    val ALMOST_EMPTY_OFFSET : Int = 5,
    val FIRST_WORD_FALL_THROUGH : Boolean = false,
    val DO_REG : Boolean = true,
    val EN_SYN : Boolean = false,
    val SRVAL : Int = 0,
    val INIT : Int = 0   

) extends BlackBox
{
  noIoPrefix()
  setDefinitionName(s"FIFO${BRAMSize}E1")
  addGeneric("DATA_WIDTH", DATA_WIDTH)
  addGeneric("ALMOST_FULL_OFFSET", ALMOST_FULL_OFFSET)
  addGeneric("ALMOST_EMPTY_OFFSET", ALMOST_EMPTY_OFFSET)
  addGeneric("FIRST_WORD_FALL_THROUGH", if(FIRST_WORD_FALL_THROUGH) "TRUE" else "FALSE")
  addGeneric("DO_REG", if(DO_REG) 1 else 0)
  addGeneric("FIFO_MODE", if(BRAMSize == 18) 
                            if(DATA_WIDTH == 36) "FIFO18_36" else "FIFO18"
                          else if(DATA_WIDTH == 72) "FIFO36_72" else "FIFO36" )
  addGeneric("EN_SYN", if(EN_SYN) "TRUE" else "FALSE" )
  addGeneric("SRVAL", SRVAL)
  addGeneric("INIT", INIT)

  val io = new Bundle {
    val DI = in Bits( (if(BRAMSize == 18) 32 else 64) bits)
    val DIP = in Bits( (if(BRAMSize == 18) 4 else 8) bits)
    val RDEN = in Bool
    val RDCLK = in Bool
    val WREN = in Bool
    val WRCLK = in Bool
    val RST = in Bool
    val RSTREG = in Bool
    val REGCE = in Bool

    val DO = out Bits( (if(BRAMSize == 18) 32 else 64) bits)
    val DOP = out Bits( (if(BRAMSize == 18) 4 else 8) bits)
    val WRCOUNT = out Bits((if(BRAMSize == 18) 12 else 13) bits)
    val RDCOUNT = out Bits((if(BRAMSize == 18) 12 else 13) bits)
    val FULL = out Bool
    val EMPTY = out Bool
    val ALMOSTFULL = out Bool
    val ALMOSTEMPTY = out Bool
    val RDERR = out Bool
    val WRERR = out Bool
  }
}

case class ASYNC_FIFO(
  val BRAMSize : Int = 18,  // 18 or 36 Kilo bits
  val dataWidth : Int = 4,  // 4, 9, 18, 36, 72 
  val outputRegister : Boolean = false,
  val almostFullOffset : Int = 5,
  val almostEmptyOffset : Int = 5,
  val firstWordFallThrough : Boolean = false
  ) extends Component
{
  var io = new Bundle{
    val readData = out Bits(dataWidth bits) 
    val writeData = in Bits(dataWidth bits)
    val full = out Bool
    val empty = out Bool
    val almostFull = out Bool
    val almostEmpty = out Bool
    val writeCount = out Bits((if(BRAMSize == 18) 12 else 13) bits)
    val readCount = out Bits((if(BRAMSize == 18) 12 else 13) bits)

    val readEnable = in Bool
    val writeEnable = in Bool

    val readClk = in Bool
    val writeClk = in Bool

    val reset = in Bool // reset must be in the slowest clock and be assert for 5 cycles
  }

  val fifo = S7_FIFO( BRAMSize = BRAMSize,
                      EN_SYN = false, 
                      DATA_WIDTH = dataWidth,
                      DO_REG = outputRegister,
                      FIRST_WORD_FALL_THROUGH = firstWordFallThrough,
                      ALMOST_FULL_OFFSET = almostFullOffset,
                      ALMOST_EMPTY_OFFSET = almostEmptyOffset )

  fifo.io.RDEN := io.readEnable
  fifo.io.WREN := io.writeEnable
  fifo.io.RST := io.reset
  fifo.io.RDCLK := io.readClk
  fifo.io.WRCLK := io.writeClk
  fifo.io.RSTREG := False // sync only
  fifo.io.REGCE := False // sync only
  io.full := fifo.io.FULL
  io.empty := fifo.io.EMPTY
  io.almostEmpty := fifo.io.ALMOSTEMPTY
  io.almostFull := fifo.io.ALMOSTFULL

  dataWidth match {
    case rw if(rw < 36) => {
      assert( rw == 4 || rw == 9 || rw == 18)
      fifo.io.DO.resize(rw) <> io.readData
      fifo.io.DI <> io.writeData(0 until rw).resize(32)
      fifo.io.DIP := 0
    }
    case 36 => {
      if(BRAMSize == 18) {
        fifo.io.DO <> io.readData(0 until 32)
        fifo.io.DI <> io.writeData(0 until 32)
        fifo.io.DOP <> io.readData(32 until 36)
        fifo.io.DIP <> io.writeData(32 until 36)
      } else
      {
        fifo.io.DO(0 until 36) <> io.readData
        fifo.io.DI <> io.writeData(0 until 36).resize(64)
        fifo.io.DIP := 0
      }
    }
    case 72 => {
      assert(BRAMSize == 36)
      fifo.io.DO <> io.readData(0 until 64)
      fifo.io.DI <> io.writeData(0 until 64)
      fifo.io.DOP <> io.readData(64 until 72)
      fifo.io.DIP <> io.writeData(64 until 72)
    }
  }
}