     hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*                                                                     ****
     h/TITLE IS215R4 - Load image queue workfile
     h option(*nodebugio:*srcstmt)
     h**************************************************************************
     h*  DESCRIPTION:
     h*
     h*  Called from:  IS215R - Work with queued index entries
     h*  Calls      :  None
     h*
     h**************************************************************************
     h*  Created on 01/25/2012 by c:282 for Project 114IAC
     h**************************************************************************
     h* Revisions:
     h*
     h* 06/10/2013 C:62 Support IM114 82447
     h*                 Change query so not to get bad conv error.
     h*
     h**************************************************************************
     c     *entry        plist
     c                   parm                    qid              10            Index Queue Id

     ** Clearing existing record from workfile IXIEP.
     c/exec sql
     c+ delete from qtemp/ixiep
     c/end-exec

     ** Load workfile IXIEP for claim index images
     c*** Begin Chg *** 06/10/13 *******************************************
     c*/exec sql
     c*+ insert into qtemp/ixiep (
     c*+ select C.IEQID, C.IEDOCT, C.IESDT, C.IESTM, C.IEDCID, C.IESTAT,
     c*+ C.IEDSTS, C.IEFL01, C.IEFL02, C.IEFL03, C.IEFL04, C.IEFl05,
     c*+ C.IERCVD, A.DODKEY, A.DODSQ#, B.CMPNAM, B.CMCOMM,
     c*+ CONCAT(CONCAT(TRIM(D.CMLSN),', '),D.CMFSN), D.CMACDT
     c*+ FROM IMDOP A, SDCMP B, IMIEP C, WMCMP D
     c*+ WHERE C.IEQID = :QID AND
     c*+       C.IEDCID = A.DODCID AND
     c*+       A.DODKEY = B.CMKEY AND
     c*+       A.DODSQ# = B.CMSEQ# AND
     c*+       B.CMPNAM = 'W0070R' AND
     c*+       D.CMCO# = DEC(SUBSTR(A.DODKEY,1,3)) AND
     c*+       D.CMFND = DEC(SUBSTR(A.DODKEY,4,3)) AND
     c*+       D.CMFYR = DEC(SUBSTR(A.DODKEY,7,3)) AND
     c*+       D.CMCASE = DEC(SUBSTR(A.DODKEY,10,7))
     c*+                         )
     c*/end-exec
     c/exec sql
     c+ insert into qtemp/ixiep (
     c+ select C.IEQID, C.IEDOCT, C.IESDT, C.IESTM, C.IEDCID, C.IESTAT,
     c+ C.IEDSTS, C.IEFL01, C.IEFL02, C.IEFL03, C.IEFL04, C.IEFl05,
     c+ C.IERCVD, A.DODKEY, A.DODSQ#, B.CMPNAM, B.CMCOMM,
     c+ CONCAT(CONCAT(TRIM(D.CMLSN),', '),D.CMFSN), D.CMACDT
     c+ FROM IMDOP A, SDCMP B, IMIEP C, WMCMP D
     c+ WHERE C.IEQID = :QID AND
     c+       C.IEDCID = A.DODCID AND
     c+       A.DODKEY = B.CMKEY AND
     c+       A.DODSQ# = B.CMSEQ# AND
     c+       B.CMPNAM = 'W0070R' AND
     c+       DIGITS(D.CMCO#) = SUBSTR(A.DODKEY,1,3) AND
     c+       DIGITS(D.CMFND) = SUBSTR(A.DODKEY,4,3) AND
     c+       DIGITS(D.CMFYR) = SUBSTR(A.DODKEY,7,3) AND
     c+       DIGITS(D.CMCASE) = SUBSTR(A.DODKEY,10,7)
     c+                         )
     c/end-exec
     c*** End   Chg *** 06/10/13 *******************************************

     ** Load workfile IXIEP for policy index images
     c/exec sql
     c+ insert into qtemp/ixiep (
     c+ select C.IEQID, C.IEDOCT, C.IESDT, C.IESTM, C.IEDCID, C.IESTAT,
     c+ C.IEDSTS, C.IEFL01, C.IEFL02, C.IEFL03, C.IEFL04, C.IEFl05,
     c+ C.IERCVD, A.DODKEY, A.DODSQ#, B.CMPNAM, B.CMCOMM,
     c+ D.EMNAME, D.EMEFD1
     c+ FROM IMDOP A, SDCMP B, IMIEP C, WMEMP D
     c+ WHERE C.IEQID = :QID AND
     c+       C.IEDCID = A.DODCID AND
     c+       A.DODKEY = B.CMKEY AND
     c+       A.DODSQ# = B.CMSEQ# AND
     c+       B.CMPNAM = 'W0050R' AND
     c+       DIGITS(D.EMCO#) = SUBSTR(A.DODKEY,1,3) AND
     c+       digits(D.EMFND) = SUBSTR(A.DODKEY,4,3) AND
     c+       digits(D.EMEMP#) = SUBSTR(A.DODKEY,7,9) AND
     c+       digits(D.EMDIV) = SUBSTR(A.DODKEY,16,5)
     c+                         )
     c/end-exec

     ** Load workfile IXIEP for un-index images
     c/exec sql
     c+ insert into qtemp/ixiep (
     c+ SELECT A.IEQID, A.IEDOCT, A.IESDT, A.IESTM, A.IEDCID, A.IESTAT,
     c+ A.IEDSTS, A.IEFL01, A.IEFL02, A.IEFL03, A.IEFL04, A.IEFL05,
     c+ A.IERCVD, ' ', 0, ' ', ' ', '***NOT INDEXED***', 0
     c+ FROM IMIEP A, IMDOP B
     c+ WHERE A.IEQID = :QID AND
     c+       A.IEDCID = B.DODCID AND
     c+       B.DOIDFX = 'N'
     c+                         )
     c/end-exec
     c                   eval      *inlr = *on
