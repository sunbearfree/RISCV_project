import java.nio.file.{Files, Paths}
import collection.mutable.HashMap

object processor_main {
  var pc: Int = 0
  var programLoopBreak: Int = 0
  var loopBreak: Int = 0

  var reg: Array[Int] = Array.fill[Int](31)(0)

  var mem: HashMap[Int,Int] = new HashMap[Int,Int]() //Memory - kunne det klares med en Byte????

  // Here the first program hard coded as an array
  var progr: HashMap[Int,Int] = new HashMap[Int,Int]()

  // self made test programs
  //var progr = Array[Int](0x00200093, 0x00300113, 0x13051200, 0x02532423)
  //var progr = Array[Int](0x7d000293, 0x02532423, 0x02832303) // load store test program
  //var progr = Array[Int](0x014002130, 0x00402023) // load store test program
  //var progr = Array[Int](0x80100293, 0x0052a023, 0x0002a303)
  //var progr = Array[Int](0xffe00093)


  // As minimal RISC-V assembler example

  def main(args: Array[String]) {
    println("Hello RISC-V World!")


    val programByteArray = Files.readAllBytes(Paths.get("addlarge.bin"))
    while (programLoopBreak < programByteArray.length) {
      val byteStr: Int = ((programByteArray(programLoopBreak + 3 & 0xff) << 24) + ((programByteArray(programLoopBreak + 2) & 0xff) << 16) + ((programByteArray(programLoopBreak + 1) & 0xff) << 8) + ((programByteArray(programLoopBreak)) & 0xff))
      progr(pc) = byteStr
      pc = pc + 1
      programLoopBreak = programLoopBreak + 4
    }

   /*      // test area begins

    val testVal: Int = 0
    println("test "+progr(testVal).toBinaryString)
    val instr: Int = progr(testVal)
    val imm_I: Int = (instr >> 20) & 0xfff
    println("test "+imm_I.toBinaryString)

    */ // test area ends

    val programLength: Int = pc
    pc = 0
    while (loopBreak == 0) {
      val instr: Int = progr(pc)
      val opcode: Int = instr & 0x7f
      val rd: Int = (instr >> 7) & 0x1f
      val func3: Int = (instr >> 12) & 0x7
      val func7: Int = (instr >> 25) & 0x7f
      val rs1: Int = (instr >> 15) & 0x1f
      val rs2: Int = (instr >> 20) & 0x1f
      val imm_I: Int = (instr >> 20) & 0xfff
      val imm_S: Int = (((instr >> 25) & 0x7f) << 5) + ((instr >> 7) & 0x1F)
      val imm_SB: Int = (((instr >> 31) & 0x1) << 12) + (((instr >> 7) & 0x1) << 11) + (((instr >> 25) & 0x3f) << 5) + ((instr >> 8) & 0xf)
      val imm_U: Int = (instr >> 12) & 0xfffff
      val imm_UJ: Int = (((instr >> 31) & 0x1) << 20) + (((instr >> 12) & 0xff) << 12) + (((instr >> 20) & 0x1) << 11) + ((instr >> 21) & 0x3ff) // er usikker på denne her. skal testes

      /*println("Opcode = " + opcode)
      println("rd = " + rd)
      println("func3 = " + func3)
      println("func7 = " + func7)
      println("rs1 = " + rs1)
      println("rs2 = " + rs2)
      println("imm_I = " + imm_I)
      println("imm_S = " + imm_S)
      println("imm_SB = " + imm_SB)
      println("imm_U = " + imm_U)
      println("imm_UJ = " + imm_UJ)*/

      opcode match {
        case 0x13 => func3 match {
          case 0x0 => reg(rd) = reg(rs1) + imm_I //ADDI
          case 0x1 => reg(rd) = reg(rs1) << imm_I //SLLI
          case 0x2 => if (reg(rd) < imm_I) { //SLTI
            reg(rd) = 1
          } else {
            reg(rd) = 0
          }
          case 0x3 => if (reg(rd) < imm_I) { //SLTIU -- Korrekt??????
            reg(rd) = 1
          } else {
            reg(rd)
          }
          case 0x4 => reg(rd) = reg(rs1) ^ imm_I //XORI
          case 0x5 => func7 match {
            case 0x0 => reg(rd) = reg(rs1) >> imm_I //SRLI
            case 0x1 => reg(rd) = reg(rs1) >>> imm_I //SRAI -- Korrekt?????
          }
          case 0x6 => reg(rd) = reg(rs1) | imm_I //ORI
          case 0x7 => reg(rd) = reg(rs1) & imm_I //ANDI
          case _ => println("ERROR3_1" + func3)
        }
        case 0x17 => reg(rd) = pc + (imm_U << 12) //AUIPC -- Korrekt?????
        case 0x33 => func3 match { //R
          case 0x0 => func7 match { //f7
            case 0x0 => reg(rd) = reg(rs1) + reg(rs2) //ADD
            case 0x20 => reg(rd) = reg(rs1) - reg(rs2) //SUB
            case _ => println("ERROR7_1" + func7)
          }
          case 0x1 => reg(rd) = reg(rs1) << reg(rs2) //SLL
          case 0x2 => if (reg(rs1) < reg(rs2)) { //SLT
            reg(rd) = 1
          } else {
            reg(rd) = 0
          }
          case 0x3 => if (reg(rs1) < reg(rs2)) { //SLTU  -- Korrekt?????
            reg(rd) = 1
          } else {
            reg(rd) = 0
          }
          case 0x4 => reg(rd) = reg(rs1) ^ reg(rs2) //XOR
          case 0x5 => func7 match { //f7
            case 0x0 => reg(rd) = reg(rs1) >>> reg(rs2) //SRL
            case 0x20 => reg(rd) = reg(rs1) >> reg(rs2) //SRA
            case _ => println("ERROR7_2" + func7)
          }
          case 0x6 => reg(rd) = reg(rs1) | reg(rs2) //OR
          case 0x7 => reg(rd) = reg(rs1) & reg(rs2) //AND
          case _ => println("ERROR3_1" + func3)
        } //R done
        case 0x37 => reg(rd) = imm_U << 12 //LUI
        case 0x23 => func3 match { // Store instructions
          case 0x0 => mem(reg(rs1) + imm_S) = reg(rs2) & 0xFF // SB
          case 0x1 => mem(reg(rs1) + imm_S) = reg(rs2) & 0xFFFF // SH
          case 0x2 => mem(reg(rs1) + imm_S) = reg(rs2) //SW
          case _ => println("ERROR3_1" + func3)
        }
        case 0x3 => func3 match { //Load instructions
          case 0x0 => reg(rd) = mem(reg(rs1) + imm_I) & 0XFF //LB
          case 0x1 => reg(rd) = mem(reg(rs1) + imm_I) & 0XFFFF //LH
          case 0x2 => reg(rd) = mem(reg(rs1) + imm_I) //LW
          case 0x4 => reg(rd) = mem(reg(rs1) + imm_I) & 0XFF //LBU -- KORREKT????
          case 0x5 => reg(rd) = mem(reg(rs1) + imm_I) & 0XFFFF //LHU -- KORREKT????
          case _ => println("ERROR3_1" + func3)
        }
        case 0x63 => func3 match {
          case 0x0 => if (reg(rs1) == reg(rs2)) { //BEQ  -- ikke testet fra her og ned \/
            pc = pc + imm_SB
          }
          case 0x1 => if (reg(rs1) != reg(rs2)) { //BNE
            pc = pc + imm_SB
          }
          case 0x4 => if (reg(rs1) < reg(rs2)) { //BLT
            pc = pc + imm_SB
          }
          case 0x5 => if (reg(rs1) >= reg(rs2)) { //BGE
            pc = pc + imm_SB
          }
          case 0x6 => if (reg(rs1) < reg(rs2)) { //BLTU   -- Korrekt?????
            pc = pc + imm_SB
          }
          case 0x7 => if (reg(rs1) >= reg(rs2)) { //BGEU   -- Korrekt?????
            pc = pc + imm_SB
          }
          case _ => println("ERROR3_1" + func3)
        }
        case 0x67 => func3 match {
          case 0x0 => {
            pc = pc + imm_I //JALR     Ikke færdig
          }
          case _ => println("ERROR3_1" + func3)
        }
        case 0x6f => {
          reg(rd) = imm_UJ //JAL
          pc = pc + imm_UJ
        }
        case _ => {
          println("Opcode " + opcode + " not yet implemented:")
          println("Opcode = " + opcode)
          println("rd = " + rd)
          println("func3 = " + func3)
          println("func7 = " + func7)
          println("rs1 = " + rs1)
          println("rs2 = " + rs2)
          println("imm_I = " + imm_I)
          println("imm_S = " + imm_S)
          println("imm_SB = " + imm_SB)
          println("imm_U = " + imm_U)
          println("imm_UJ = " + imm_UJ)
        }
      }
      pc = pc + 1
      for (i <- reg.indices) print(reg(i) + " ") // reg.indices = 0 until reg.length
      println()
      if (pc >= programLength) {
        loopBreak = 1
      }
    }
    println("Program exit")
  }
}
