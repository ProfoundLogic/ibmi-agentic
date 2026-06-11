     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H/define CONVERT
     H/TITLE SS015R -
      ********************************************************************
      * Revisions:
      *
      * 03/17/13  C:33  Remove requirement for GS
      *                 - No comments (check prior release)
      *
      * 09/16/11  C:33  Project: SY180   OP025/77775
      *                 Do not update PR password with each transaction.
      *
      * 01/28/10  C:XXX  Project: SY215                               *
      *           C:189  Retrieve brand Name
      *
      * 04/03/09  C:61  Project sy180
      *                 Add processing for two additional license keys.
      *
      ********************************************************************
     Fssypf     uf   e             disk    usropn

     DPgmStsDS       esds                  extname(szq1p)
     DtwentyZero       s             20    inz('00000000000000000000')
     D pdsName         S             40    Inz
     D COMAPI          c                   'WA001R'
     DprogTitle        c                   'Password for PDS applications'

     D##ser#           s              8
     D##expDate        s               d
     D##entDate        s               d
     Dxxdays           s              5  0
     D##model          s              4
     D##error          s              1
     DcurPasWrd        s             32
     DxxCurr           s              1    inz('N')
     DxxMach           s              2    inz('PR')
     DxxSer#           s              7
     DxxModel          s              4
     DxxExpDate        s               d   inz(d'2099-12-31')
     DxxEntDate        s               d
     DxxRtCd           s              5

     D                 ds
     DxxPasWrd                       32
     D  scPasWrd1                     4    overlay(xxPasWrd:1)
     D  scPasWrd2                     4    overlay(xxPasWrd:5)
     D  scPasWrd3                     4    overlay(xxPasWrd:9)
     D  scPasWrd4                     4    overlay(xxPasWrd:13)
     D  scPasWrd5                     4    overlay(xxPasWrd:17)
     D  scPasWrd6                     4    overlay(xxPasWrd:21)
     D  scPasWrd7                     4    overlay(xxPasWrd:25)
     D  scPasWrd8                     4    overlay(xxPasWrd:29)

     Dpassword         ds
     D  syal01                       10
     D  syal02                       10
     D  syal03                       10
     D  syal04                       10

     DpassWordQA       ds
     D  syqa01                       10
     D  syqa02                       10
     D  syqa03                       10
     D  syqa04                       10

     DpassWordDR       ds
     D  sydr01                       10
     D  sydr02                       10
     D  sydr03                       10
     D  sydr04                       10

     DbrandName        s             80    inz

     D/copy SPrRtvBrnd


     D*=====================================================================
     D* Procedure prototypes
     D*=====================================================================
     DCvtToDate        PR              d
     D numDate                        8  0 Const

     DChkPasswrd       PR             1
     D inPasWrd                      32    Const
     D outSerNbr                      7
     D outExpDate                      d
     D outEntDate                      d
     D outDays                        5  0
     D outModel                       4
     D inCurr                         1

     DErrorCheck       pr             1n

     DGetPasswrd       PR            32
     D SerNumber                      7    Const
     D ExpDate                         d   Const
     D CurDate                         d   Const
     D Model                          4    Const options(*nopass)

     DGetSerial#       pr                  extPgm('SS011C1')
     D outSerial#                     8

     DSndPgmMsg        PR                  opdesc
     D msgInId                       10    const
     D msgInType                     10    const
     D msgFile                       10    const
     D msgLib                        10    const
     D msgInData                  32766    const options(*varsize:*nopass)

      *=====================================================================
      * PLIST
      *=====================================================================

     C     *entry        plist
     C                   parm                    @@mach            2
     C                   parm                    @@ser#            8
     C                   parm                    @@passWord       34
     C                   parm                    @@model           4
     C                   parm                    @@expDate         7
     C                   parm                    @@RtCd            5

      *=====================================================================
      * BEGIN
      *=====================================================================

      /Free
       If %parms <= 0;
          SndPgmMsg('CPF9898':'*ESCAPE':'QCPFMSG':'*LIBL'
           :'Command ADDPDLIC needs to be used');
          *inlr = *on;
          Return;
       Endif;

       If %parms >= 6;
          xxRtCd = @@RtCd;
       Endif;

       xxMach = @@mach;
       xxSer# = @@ser#;
       xxModel = @@model;
       xxPasWrd = %subst(@@password:3:32);

       If @@expDate = *zero;
          @@expDate = '1991231';
       Endif;
       xxExpDate = %date(%int(@@expDate):*cymd);

       If xxSer# = *blank;
          GetSerial#(##Ser#);
          xxSer# = %subst(##Ser#:2:7);
          xxCurr = 'Y';
       Endif;

       // If no errors, process information
       Select;
       When xxRtCd = '*ECHK' and ErrorCheck = *on;
          @@RtCd = '*PASS';
          *inlr = *on;
          Return;
       When xxRtCd = '*ECHK';
          Clear @@RtCd;
          *inlr = *on;
          Return;
       When ErrorCheck = *on;
          SndPgmMsg('CPF9898':'*ESCAPE':'QCPFMSG':'*LIBL'
           :'Invalid password entered. ' + xxPasWrd);
          *inlr = *on;
          Return;
       Endsl;

       Open ssypf;
       Chain 1 ssypf;
       If %found;
          Select;
          When xxMach = 'PR';
             passWord = xxPasWrd;
          When xxMach = 'QA';
             passWordQA = xxPasWrd;
          When xxMach = 'DR';
             passWordDR = xxPasWrd;
          Endsl;
          Update ssy01;
       Endif;
       Close ssypf;

       *inlr = *on;
      /End-free

     P*=====================================================================
     P* errorCheck - check for errors on screen
     P*=====================================================================
     PErrorCheck       b
     DErrorCheck       pi             1n

      /Free
       If xxPasWrd = *blanks;
          Return *on;
       Endif;

       If ChkPassWrd(xxPasWrd:xxSer#:xxExpDate:xxEntDate:xxDays:xxModel
        :xxCurr) = *on;
          Return *on;
       Endif;

       If %date <> xxEntDate;
          Return *on;
       Endif;

       Return *off;

      /End-free
     PErrorCheck       e

     P*=====================================================================
     P* SndPgmMsg - sends a program message to the current program
     P*             message queue.
     P*  Parameter 1 - 10 character message ID
     P*  Parameter 2 - 10 character message type
     P*  Parameter 3 - 10 character message file
     P*  Parameter 4 - 10 character message file library
     P*  Parameter 5 - varible character message data
     P*  Returns - nothing
     P*=====================================================================
     PSndPgmMsg        B                   Export
     DSndPgmMsg        PI                  opdesc
     D msgInId                       10    const
     D msgInType                     10    const
     D msgFile                       10    const
     D msgLib                        10    const
     D msgInDta                   32766    const options(*varsize:*nopass)
     D
     DCEEDOD           PR
     D parmNum                       10I 0 const
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               12A   options(*omit)
     D*
     D* parameters passed to CEEDOD
     D*
     D descType        s             10I 0
     D dataType        s             10I 0
     D descInfo1       s             10I 0
     D descInfo2       s             10I 0
     D inLen           s             10I 0
     D hexLen          s             10I 0
     D
     D msgData         s                   like(msgInDta) inz
     D msgDtaLen       s              6  0 inz
     D msgFileLib      s             20
     D msgID           s             10
     D msgKey          s              9b 0
     D msgQueue        s             10
     D msgQueNbr       s              9b 0
     D msgType         s             10
     D
     DErrorDS          DS                  inz
     D bytesProv               1      4b 0
     D bytesAval               5      8b 0
     D messageID               9     15
     D err##                  16     16
     D messageDta             17    116
     D
      /Free
       msgId = msgInId;
       msgType = msgInType;
       msgFileLib = msgFile + msgLib;

       If %parms >= 5;
          CEEDOD(5:descType:dataType:descInfo1:descInfo2:inLen:*omit);
          msgDtaLen = inLen;
          msgData = msgInDta;
       Endif;

       Select;
       When msgType = '*STATUS';
          msgQueue = '*EXT';
       Other;
          msgQueue = '*';
       Endsl;

      /End-free
     C                   call      'QMHSNDPM'
     C                   parm                    msgId
     C                   parm                    msgFileLib
     C                   parm                    msgData
     C                   parm                    msgDtaLen
     C                   parm                    msgType
     C                   parm                    msgQueue
     C                   parm      1             msgQueNbr
     C                   parm                    msgKey
     C                   parm                    errorDS
     C
     C* *                call      'SNDPGMMSG'
     C* *                parm                    msgId
     C* *                parm                    msgData
     C* *                parm                    msgType
     C
     C                   return
     PSndPgmMsg        E
