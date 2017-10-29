--
-- Copyright 2017 Warlock <internalmike@gmail.com>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--

module Data.Pmevm.Monitor.Native where

#include <haskell>
import Data.Pmevm

monitor :: Program
monitor = Program
  [ (0o000, 0o000, 0o303, "          JMP M1            ")
  , (0o000, 0o001, 0o070, "                            ")
  , (0o000, 0o002, 0o000, "                            ")
  , (0o000, 0o070, 0o061, "      M1: LXI SP 010000Q    ")
  , (0o000, 0o071, 0o000, "                            ")
  , (0o000, 0o072, 0o020, "                            ")
  , (0o000, 0o073, 0o041, "          LXI HL 006000Q    ")
  , (0o000, 0o074, 0o000, "                            ")
  , (0o000, 0o075, 0o014, "                            ")
  , (0o000, 0o076, 0o116, "      M2: MOV C M           ")
  , (0o000, 0o077, 0o174, "          MOV A H           ")
  , (0o000, 0o100, 0o323, "          OUT 002Q          ")
  , (0o000, 0o101, 0o002, "                            ")
  , (0o000, 0o102, 0o175, "          MOV A L           ")
  , (0o000, 0o103, 0o323, "          OUT 001Q          ")
  , (0o000, 0o104, 0o001, "                            ")
  , (0o000, 0o105, 0o171, "      M3: MOV A C           ")
  , (0o000, 0o106, 0o323, "          OUT 000Q          ")
  , (0o000, 0o107, 0o000, "                            ")
  , (0o000, 0o110, 0o315, "      M4: CALL SKL          ")
  , (0o000, 0o111, 0o177, "                            ")
  , (0o000, 0o112, 0o000, "                            ")
  , (0o000, 0o113, 0o376, "          CPI 010Q          ")
  , (0o000, 0o114, 0o010, "                            ")
  , (0o000, 0o115, 0o322, "          JNC M11           ")
  , (0o000, 0o116, 0o134, "                            ")
  , (0o000, 0o117, 0o000, "                            ")
  , (0o000, 0o120, 0o107, "          MOV E A           ")
  , (0o000, 0o121, 0o171, "          MOV A C           ")
  , (0o000, 0o122, 0o027, "          RAL               ")
  , (0o000, 0o123, 0o027, "          RAL               ")
  , (0o000, 0o124, 0o027, "          RAL               ")
  , (0o000, 0o125, 0o346, "          ANI 370Q          ")
  , (0o000, 0o126, 0o370, "                            ")
  , (0o000, 0o127, 0o260, "          OR B              ")
  , (0o000, 0o130, 0o117, "          MOV C A           ")
  , (0o000, 0o131, 0o303, "          JMP M3            ")
  , (0o000, 0o132, 0o105, "                            ")
  , (0o000, 0o133, 0o000, "                            ")
  , (0o000, 0o134, 0o376, "     M11: CPI 010Q          ")
  , (0o000, 0o135, 0o010, "                            ")
  , (0o000, 0o136, 0o302, "          JNZ M12           ")
  , (0o000, 0o137, 0o145, "                            ")
  , (0o000, 0o140, 0o000, "                            ")
  , (0o000, 0o141, 0o141, "          MOV H C           ")
  , (0o000, 0o142, 0o303, "          JMP M2            ")
  , (0o000, 0o143, 0o076, "                            ")
  , (0o000, 0o144, 0o000, "                            ")
  , (0o000, 0o145, 0o376, "     M12: CPI 011Q          ")
  , (0o000, 0o146, 0o011, "                            ")
  , (0o000, 0o147, 0o302, "          JNZ M13           ")
  , (0o000, 0o150, 0o156, "                            ")
  , (0o000, 0o151, 0o000, "                            ")
  , (0o000, 0o152, 0o151, "          MOV L C           ")
  , (0o000, 0o153, 0o303, "          JMP M2            ")
  , (0o000, 0o154, 0o076, "                            ")
  , (0o000, 0o155, 0o000, "                            ")
  , (0o000, 0o156, 0o376, "     M13: CPI 012Q          ")
  , (0o000, 0o157, 0o012, "                            ")
  , (0o000, 0o160, 0o302, "          JNZ M14           ")
  , (0o000, 0o161, 0o170, "                            ")
  , (0o000, 0o162, 0o000, "                            ")
  , (0o000, 0o163, 0o161, "          MOV M C           ")
  , (0o000, 0o164, 0o043, "          INX HL            ")
  , (0o000, 0o165, 0o303, "          JMP M2            ")
  , (0o000, 0o166, 0o076, "                            ")
  , (0o000, 0o167, 0o000, "                            ")
  , (0o000, 0o170, 0o376, "     M14: CPI 013Q          ")
  , (0o000, 0o171, 0o013, "                            ")
  , (0o000, 0o172, 0o302, "          JNZ M4            ")
  , (0o000, 0o173, 0o110, "                            ")
  , (0o000, 0o174, 0o000, "                            ")
  , (0o000, 0o175, 0o351, "          PCHL              ")
  , (0o000, 0o176, 0o000, "                            ")
  , (0o000, 0o177, 0o076, "     SKL: MVI A 000Q        ")
  , (0o000, 0o200, 0o000, "                            ")
  , (0o000, 0o201, 0o323, "          OUT 003Q          ")
  , (0o000, 0o202, 0o003, "                            ")
  , (0o000, 0o203, 0o333, "      M5: IN 003Q           ")
  , (0o000, 0o204, 0o003, "                            ")
  , (0o000, 0o205, 0o346, "          ANI 017Q          ")
  , (0o000, 0o206, 0o017, "                            ")
  , (0o000, 0o207, 0o376, "          CPI 017Q          ")
  , (0o000, 0o210, 0o017, "                            ")
  , (0o000, 0o211, 0o302, "          JNZ M5            ")
  , (0o000, 0o212, 0o203, "                            ")
  , (0o000, 0o213, 0o000, "                            ")
  , (0o000, 0o214, 0o315, "          CALL DL           ")
  , (0o000, 0o215, 0o277, "                            ")
  , (0o000, 0o216, 0o000, "                            ")
  , (0o000, 0o217, 0o325, "          PUSH DE           ")
  , (0o000, 0o220, 0o026, "      M8: MVI D 003Q        ")
  , (0o000, 0o221, 0o003, "                            ")
  , (0o000, 0o222, 0o036, "          MVI E 376Q        ")
  , (0o000, 0o223, 0o376, "                            ")
  , (0o000, 0o224, 0o173, "      M7: MOV A E           ")
  , (0o000, 0o225, 0o323, "          OUT 003Q          ")
  , (0o000, 0o226, 0o003, "                            ")
  , (0o000, 0o227, 0o007, "          RLC               ")
  , (0o000, 0o230, 0o137, "          MOV E A           ")
  , (0o000, 0o231, 0o333, "          IN 003Q           ")
  , (0o000, 0o232, 0o003, "                            ")
  , (0o000, 0o233, 0o346, "          ANI 017Q          ")
  , (0o000, 0o234, 0o017, "                            ")
  , (0o000, 0o235, 0o376, "          CPI 017Q          ")
  , (0o000, 0o236, 0o017, "                            ")
  , (0o000, 0o237, 0o302, "          JNZ M6            ")
  , (0o000, 0o240, 0o254, "                            ")
  , (0o000, 0o241, 0o000, "                            ")
  , (0o000, 0o242, 0o025, "          DCR D             ")
  , (0o000, 0o243, 0o172, "          MOV A D           ")
  , (0o000, 0o244, 0o376, "          CPI 377Q          ")
  , (0o000, 0o245, 0o377, "                            ")
  , (0o000, 0o246, 0o302, "          JNZ M7            ")
  , (0o000, 0o247, 0o224, "                            ")
  , (0o000, 0o250, 0o000, "                            ")
  , (0o000, 0o251, 0o303, "          JMP M8            ")
  , (0o000, 0o252, 0o220, "                            ")
  , (0o000, 0o253, 0o000, "                            ")
  , (0o000, 0o254, 0o315, "      M6: CALL DL           ")
  , (0o000, 0o255, 0o277, "                            ")
  , (0o000, 0o256, 0o000, "                            ")
  , (0o000, 0o257, 0o017, "     M10: RRC               ")
  , (0o000, 0o260, 0o322, "          JNC M9            ")
  , (0o000, 0o261, 0o274, "                            ")
  , (0o000, 0o262, 0o000, "                            ")
  , (0o000, 0o263, 0o365, "          PUSH PSWA         ")
  , (0o000, 0o264, 0o172, "          MOV A D           ")
  , (0o000, 0o265, 0o306, "          ADI 004Q          ")
  , (0o000, 0o266, 0o004, "                            ")
  , (0o000, 0o267, 0o127, "          MOV D A           ")
  , (0o000, 0o270, 0o361, "          POP PSWA          ")
  , (0o000, 0o271, 0o303, "          JMP M10           ")
  , (0o000, 0o272, 0o257, "                            ")
  , (0o000, 0o273, 0o000, "                            ")
  , (0o000, 0o274, 0o172, "      M9: MOV A D           ")
  , (0o000, 0o275, 0o321, "          POP DE            ")
  , (0o000, 0o276, 0o311, "          RET               ")
  , (0o000, 0o277, 0o365, "      DL: PUSH PSWA         ")
  , (0o000, 0o300, 0o325, "          PUSH DE           ")
  , (0o000, 0o301, 0o021, "          LXI D 001016Q     ")
  , (0o000, 0o302, 0o016, "                            ")
  , (0o000, 0o303, 0o002, "                            ")
  , (0o000, 0o304, 0o033, "       N: DCX DE            ")
  , (0o000, 0o305, 0o172, "          MOV A D           ")
  , (0o000, 0o306, 0o263, "          ORA E             ")
  , (0o000, 0o307, 0o302, "          JNZ N             ")
  , (0o000, 0o310, 0o304, "                            ")
  , (0o000, 0o311, 0o000, "                            ")
  , (0o000, 0o312, 0o321, "          POP DE            ")
  , (0o000, 0o313, 0o361, "          POP PSWA          ")
  , (0o000, 0o314, 0o311, "          RET               ")
  ]
