**FREE
// ---------------------------------------------------------------------------
// LOCKMSG - report a record lock / file error to the operator.
//
// RECONSTRUCTED HELPER.  PODTLUI calls LOCKMSG(STATUSS) after a failed update,
// passing its 333 byte program status data structure.  The useful fields are
// the failing procedure, the CPF message id at offset 40 and its text at 91.
// ---------------------------------------------------------------------------
CTL-OPT OPTION(*SRCSTMT:*NODEBUGIO);
CTL-OPT DFTACTGRP(*NO) ACTGRP(*CALLER);
CTL-OPT MAIN(Main);

DCL-PR SndPgmMsg EXTPGM('QMHSNDPM');
  MsgId      CHAR(7)   CONST;
  MsgFile    CHAR(20)  CONST;
  MsgData    CHAR(512) CONST OPTIONS(*VARSIZE);
  MsgDataLen INT(10)   CONST;
  MsgType    CHAR(10)  CONST;
  StackEntry CHAR(10)  CONST;
  StackCount INT(10)   CONST;
  MsgKey     CHAR(4);
  ErrorCode  CHAR(8);
END-PR;

DCL-PROC Main;
  DCL-PI *N EXTPGM('LOCKMSG');
    @STATUS CHAR(333) OPTIONS(*VARSIZE);
  END-PI;

  DCL-S TEXT   CHAR(512);
  DCL-S KEY    CHAR(4);
  DCL-S ERRC   CHAR(8) INZ(*ALLX'00');
  DCL-S CPFID  CHAR(7);
  DCL-S CPFTXT CHAR(80);

  CPFID  = %SUBST(@STATUS : 40 : 7);
  CPFTXT = %SUBST(@STATUS : 91 : 80);

  IF CPFID = *BLANKS AND CPFTXT = *BLANKS;
    TEXT = 'Record is locked by another job - try again.';
  ELSE;
    TEXT = %TRIM(CPFID) + ': ' + %TRIM(CPFTXT);
  ENDIF;

  SndPgmMsg('CPF9897' : 'QCPFMSG   *LIBL     ' : TEXT : %LEN(%TRIM(TEXT))
           : '*DIAG' : '*PGMBDY' : 1 : KEY : ERRC);
END-PROC;
