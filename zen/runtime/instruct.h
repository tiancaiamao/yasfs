#ifndef _INSTRUCT_H
#define _INSTRUCT_H

enum{
  STOP    = 1,
  PUSHADDR = 2,
  CONST   = 3,
  PUSH    = 4,
  CLOSURE = 5,
  APPLY   = 6,
  GRAB    = 7,
  RESTART  = 8,
  STACKACC = 9,
  ENVACC  = 10,
  ADDINT  = 11,
  RETURN  = 12,
  BRANCH  = 13,
  BRANCHIF = 14,
  EQ      = 15,
  SUBINT  = 16,
  MULINT  = 17,
  DIVINT  = 18,
};

#endif
