/* @MBRTEXT: Send Display Message */
/* @MBRTYPE: CLP */
/********************************************************************/
/* RTC Group, Inc.                                                  */
/* Retrieve message from message file and send to caller's          */
/*   program queue                                                  */
/*                                                                  */
/* Log:                                                             */
/* 08/01/01  Billy Andrews  Created                                 */
/*                                                                  */
/********************************************************************/

             PGM        PARM(&MSGF &MSGID &CALLPROC &CALLMOD &CALLPGM)

             DCL        VAR(&MSGF) TYPE(*CHAR) LEN(10)
             DCL        VAR(&MSGID) TYPE(*CHAR) LEN(7)
             DCL        VAR(&CALLPROC) TYPE(*CHAR) LEN(10)
             DCL        VAR(&CALLMOD) TYPE(*CHAR) LEN(10)
             DCL        VAR(&CALLPGM) TYPE(*CHAR) LEN(10)

             SNDPGMMSG  MSGID(&MSGID) MSGF(&MSGF) TOPGMQ(*SAME +
                          (&CALLPROC &CALLMOD &CALLPGM))
             MONMSG     MSGID(CPF0001 CPF2469) EXEC(DO)
             SNDPGMMSG  MSG('Message ID' *BCAT &MSGID *BCAT +
                          'not found in message file' *BCAT &MSGF) +
                        TOPGMQ(*SAME (&CALLPROC &CALLMOD &CALLPGM))
             ENDDO

             ENDPGM

