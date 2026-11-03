/* @MBRTEXT: Clear calling program's message queue */
/* @MBRTYPE: CLP */
/********************************************************************/
/* RTC Group, Inc.                                                  */
/* Clear caller's message queue                                     */
/*                                                                  */
/* Log:                                                             */
/* 08/01/01  Billy Andrews  Created                                 */
/*                                                                  */
/********************************************************************/

             PGM        PARM(&CALLPROC &CALLMOD &CALLPGM)

             DCL        VAR(&CALLPROC) TYPE(*CHAR) LEN(10)
             DCL        VAR(&CALLMOD) TYPE(*CHAR) LEN(10)
             DCL        VAR(&CALLPGM) TYPE(*CHAR) LEN(10)

             RMVMSG     PGMQ(*SAME (&CALLPROC &CALLMOD &CALLPGM)) +
                          CLEAR(*ALL)

             ENDPGM

