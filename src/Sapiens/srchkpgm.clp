/* (c) 2012 StoneRiver, Inc.  All rights reserved.                   */
/* StoneRiver is a trademark of StoneRiver, Inc.                     */
             PGM        PARM(&PROGNAME &FLAG)
             DCL        VAR(&PROGNAME) TYPE(*CHAR) LEN(10)
             DCL        VAR(&FLAG) TYPE(*LGL) LEN(1)
             DCL        VAR(&MSGKEY) TYPE(*CHAR) LEN(4)

             DCLPRCOPT  DFTACTGRP(*NO) ACTGRP(*CALLER)

             CHGVAR     VAR(&FLAG) VALUE('1')
             SNDPGMMSG  MSG('This message is to see if this program +
                          exists in the program stack.') +
                          TOPGMQ(*SAME &PROGNAME) KEYVAR(&MSGKEY)
             MONMSG     MSGID(CPF0000) EXEC(DO)
             CHGVAR     VAR(&FLAG) VALUE('0')
             RETURN
             ENDDO

             RMVMSG     PGMQ(*SAME &PROGNAME) MSGKEY(&MSGKEY)
             ENDPGM
