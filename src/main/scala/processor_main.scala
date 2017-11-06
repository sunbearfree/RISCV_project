import scala.util.control.Breaks._
import java.nio.file.{Files, Paths}

object processor_main {
  var pc: Int = 0
  var lc: Int = 0

  val reg: Array[Int] = Array.fill[Int](31)(0)

  // Here the first program hard coded as an array
  var progr = Array[Int](0x00200093, 0x00300113, 0x13051200)
  //val byteArrayTest = Files.readAllLines(Paths.get("branchcnt.bin"))
  // As minimal RISC-V assembler example

  def main(args: Array[String]){
    println("Hello RISC-V World!")
    println(byteArrayTest(1))
    pc = 0
    while (lc==0) {
      val instr: Int = progr(pc)
      val opcode: Int = instr & 0x7f
      val rd: Int = (instr >> 7) & 0x1f
      val func3: Int = (instr >> 12) & 0x7
      val rs1: Int = (instr >> 15) & 0x1f
      val rs2: Int = (instr >> 20) & 0x1f
      val imm0: Int = (instr >> 20) & 0xfff
      val func7: Int = (instr >> 25) & 0x7f
      val imm1: Int = (instr >> 25) & 0x7f

      opcode match {
        case 0x13 => reg(rd) = reg(rs1) + imm0                      //ADDI
        case 0x33 => func3 match {                           //R
          case 0x0 => func7 match {                    //f7
            case 0x0 => reg(rd) = reg(rs1) + reg(rs2)               //ADD
            case 0x20 => reg(rd) = reg(rs1) - reg(rs2)              //SUB
            case _ => println("ERROR7_1"+func7)
          }
          case 0x1 => reg(rd) = reg(rs1) << reg(rs2)                //SLL
          case 0x2 => if(reg(rs1)<reg(rs2)){                        //SLT
            reg(rd) = 1
          } else{
            reg(rd) = 0
          }
          case 0x3 => if(reg(rs1)<(imm0&0xFF)){                     //SLTU
            reg(rd) = 1
          } else{
            reg(rd) = 0
          }
          case 0x4 => reg(rd) = reg(rs1) ^ reg(rs2)                 //SLTU
          case 0x5 => func7 match {                    //f7
            case 0x0 => reg(rd) = reg(rs1) >>> reg(rs2)             //SRL
            case 0x20 => reg(rd) = reg(rs1) >> reg(rs2)             //SRA
            case _ => println("ERROR7_2"+func7)
          }
          case 0x6 => reg(rd) = reg(rs1) | reg(rs2)                 //OR
          case 0x7 => reg(rd) = reg(rs1) & reg(rs2)                 //AND
          case _ => println("ERROR3_1"+func3)
        }                                                    //R done
        case 0x37 => reg(rd) = imm0<<16                             //LUI
        case _ => {
          println("Opcode " + opcode + " not yet implemented:")
          println("Opcode = " + opcode)
          println("rd = " + rd)
          println("func3 = " + func3)
          println("rs1 = " + rs1)
          println("rs2 = " + rs2)
          println("imm0 = " + imm0)
          println("func7 = " + func7)
          println("imm1 = " + imm1)
        }
      }
      pc = pc + 1
      for (i <- 0 until reg.length) print(reg(i) + " ")
      println()
      if (pc >= progr.length) {lc = 1}
    }
  }
  println("Program exit")
}
