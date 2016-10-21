#ifndef _INSTRUCT_H
#define _INSTRUCT_H

enum{
  STOP    = 1,
  MARK    = 2,
  CONST   = 3,
  PUSH    = 4,
  CLOSURE = 5,
  APPLY   = 6,
  CHECK   = 7,
  ENV     = 8,
  ACCESS  = 9,
  ADDINT  = 10,
  UNENV   = 11,
  RETURN  = 12,
  BRANCH  = 13,
};

#endif
