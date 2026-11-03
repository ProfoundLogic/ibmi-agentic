/* @MBRTEXT: INVENTORY BALANCE INQUIRY - VW WEB (EJS) VERSION */
/* @MBRTYPE: CLP */
/* VW01 11/03/26 Web clone of INV046CL for menu option 10: calls INV046W */
/*      (the same program as INV046, on the EJS display file INV046WD). */
             PGM
             DCL        VAR(&INUMBR) TYPE(*DEC) LEN(9 0) VALUE(0)
             CALL       PGM(INV046W) PARM(&INUMBR)
             ENDPGM
