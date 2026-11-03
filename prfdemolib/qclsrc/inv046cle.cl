/* @MBRTEXT: INVENTORY BALANCE INQUIRY - PLAIN WEB (EJS) VERSION */
/* @MBRTYPE: CLP */
/* GENERATED from inv046clw.cl by prfdemolib/tools/gen-plain-clones.py */
/* VW01 11/03/26 Plain clone of INV046CL for menu option 7: calls INV046E */
/*      (the same program as INV046, on the EJS display file INV046ED). */
             PGM
             DCL        VAR(&INUMBR) TYPE(*DEC) LEN(9 0) VALUE(0)
             CALL       PGM(INV046E) PARM(&INUMBR)
             ENDPGM
