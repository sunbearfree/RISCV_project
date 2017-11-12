import scala.util.control.Breaks._
import java.nio.file.{Files, Paths}
import java.util
import collection.mutable.HashMap

object processor_main {
  var pc: Int = 0
  var lc: Int = 0

  val reg: Array[Int] = Array.fill[Int](31)(0)

  var mem: HashMap[Int,Int] = new HashMap[Int,Int]() //Memory - kunne det klares med en Byte????

  // Here the first program hard coded as an array
//  var progr = Array[Int](0x00200093, 0x00300113, 0x13051200, 0x02532423)
//var progr = Array[Int](0x7d000293, 0x02532423, 0x02832303) // load store test program
var progr = Array[Int](0xfec00293, 0x02532423, 0x02834303) // load store test program

  //val byteArrayTest = Files.readAllLines(Paths.get("branchcnt.bin"))
  // As minimal RISC-V assembler example

  def main(args: Array[String]){
    println("Hello RISC-V World!")
    //println(byteArrayTest(1))
    pc = 0
    while (lc==0) {
      val instr: Int = progr(pc)
      val opcode: Int = instr & 0x7f
      val rd: Int = (instr >> 7) & 0x1f
      val func3: Int = (instr >> 12) & 0x7
      val func7: Int = (instr >> 25) & 0x7f
      val rs1: Int = (instr >> 15) & 0x1f
      val rs2: Int = (instr >> 20) & 0x1f
      val imm_I20: Int = (instr >>> 20) & 0xfff // Kan dette og de andre gøres mere effektikvt
      val imm_S25: Int = (instr >> 25) & 0x7f

      opcode match {
        case 0x13 => func3 match {
          case 0x0 => reg(rd) = reg(rs1) + imm_I20                      //ADDI
          case 0x1 => reg(rd) = reg(rs1) << imm_I20                     //SLLI
          case 0x2 => if (reg(rd)<reg(rs1)) {                           //SLTI
            reg(rd) = 1
          } else {
            reg(rd) = 0
          }
          case 0x3 => if (reg(rd)<reg(rs1)) {                           //SLTIU -- Korrekt??????
            reg(rd) = 1
          } else {
            reg(rd)
          }
          case 0x4 => reg(rd) = reg(rs1) ^ imm_I20                      //XORI
          case 0x5 => func7 match {
            case 0x0 => reg(rd) = reg(rs1) >> imm_I20                      //SRLI
            case 0x1 => reg(rd) = reg(rs1) >>> imm_I20                      //SRAI -- Korrekt?????
          }
          case 0x6 => reg(rd) = reg(rs1) | imm_I20                      //ORI
          case 0x7 => reg(rd) = reg(rs1) & imm_I20                      //ANDI
        }
        case 0x17 => reg(rd) = pc + imm_I20                              //AUIPC -- Korrekt?????
        case 0x33 => func3 match {                           //R
          case 0x0 => func7 match {                    //f7
            case 0x0 => reg(rd) = reg(rs1) + reg(rs2)               //ADD
            case 0x4 => reg(rd) = reg(rs1)^reg(rs2)                 //XOR
            case 0x20 => reg(rd) = reg(rs1) - reg(rs2)              //SUB
            case _ => println("ERROR7_1"+func7)
          }
          case 0x1 => reg(rd) = reg(rs1) << reg(rs2)                //SLL
          case 0x2 => if(reg(rs1)<reg(rs2)){                        //SLT
            reg(rd) = 1
          } else{
            reg(rd) = 0
          }
          case 0x3 => if(reg(rs1)<(imm_I20&0xFF)){                     //SLTU
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
        case 0x37 => reg(rd) = imm_I20<<16                             //LUI
        case 0x23 => func3 match {                            // Store instructions
          case 0x0 => mem(reg(rs1)+imm_S25) = reg(rs2)&0xFF // SB
          case 0x1 => mem(reg(rs1)+imm_S25) = reg(rs2)&0xFFFF// SH
          case 0x2 => mem(reg(rs1)+imm_S25) = reg(rs2)    //SW
        }
        case 0x3 => func3 match {                             //Load instructions
          case 0x0 => reg(rd) = mem(reg(rs1)+ imm_S25)&0XFF //LB
          case 0x1 => reg(rd) = mem(reg(rs1)+ imm_S25)&0XFFFF //LH
          case 0x2 => reg(rd) = mem(reg(rs1)+ imm_S25) //LW
          case 0x4 => reg(rd) = mem(reg(rs1)+ imm_S25)&0XFF //LBU -- KORREKT????
          case 0x5 => reg(rd) = mem(reg(rs1)+ imm_S25)&0XFFFF //LHU -- KORREKT????
        }

        case _ => {
          println("Opcode " + opcode + " not yet implemented:")
          println("Opcode = " + opcode)
          println("rd = " + rd)
          println("func3 = " + func3)
          println("rs1 = " + rs1)
          println("rs2 = " + rs2)
          println("imm_I20 = " + imm_I20)
          println("func7 = " + func7)
          println("imm_S25 = " + imm_S25)
        }
      }
      pc = pc + 1
      for (i <- reg.indices) print(reg(i).toHexString + " ") // reg.indices = 0 until reg.length
      println()
      if (pc >= progr.length) {lc = 1}
    }
  }
  println("Program exit")
}
