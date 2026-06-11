     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
      //*********************************************************************
      * Taking a stream file from the IFS and dumping
      * it (as-is) to a *USERASCII spooled file.
      *
      * For this PDF Print (any stream print):
      *            * MGLIB/PRTSTMFR  - RPG PGM
      *            * MGLIB/PRTSTMF   - Cmd
      *            * MGLIB/IFSIO_H   -  RPG Source
      *
      *****   if invalid outqueue - send to QPRINT as default
      *
      * To compile:
      *    CRTBNDRPG PGM(xxlib/PRTSTMFR) SRCFILE(xxLib/QRPGLESRC)
      *    CRTCMD CMD(xxlib/PRTSTMF) SRCFILE(xxlib/QCMDSRC)
      *           PGM(xxlib/PRTSTMFR)
      *
      * To run:
      *    PRTSTMF STMF('/home/MyDirHere/test.pdf') OUTQ(PRT01)
      *
     H BNDDIR('QC2LE')
     h dftactgrp(*no) actgrp(*caller)
      //*********************************************************************
      //===============================================================*
      //  Created   : 12/20/16                                         *
      //  Programmer: C:65                                             *
      //  Project   : SY371 - Print command for sending PDF documents  *
      //                      to a printer via an out queue.           *
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      //                                                               *
      //===============================================================*

     FPRTSTMFF  O    F  132        PRINTER USROPN

     D PRTSTMF         PR                  ExtPgm('PRTSTMF')
     D   stream                     256A   varying const
     D   outq                        10A   const
     D   formtype                    10A   const
     D   hold                         4A   const
     D   save                         4A   const
     D   copies                       3P 0 const
     D PRTSTMF         PI
     D   stream                     256A   varying const
     D   outq                        10A   const
     D   formtype                    10A   const
     D   hold                         4A   const
     D   save                         4A   const
     D   copies                       3P 0 const

      /copy IFSIO_H

     D QCMDEXC         PR                  ExtPgm('QCMDEXC')
     D   command                  32702A   const options(*varsize)
     D   len                         15P 5 const
     D   igc                          3a   const options(*nopass)

     D ReportError     PR

     D cmd             s            500A   varying
     D fd              s             10I 0
     D buf             s            132A

      /free

         cmd = 'OVRPRTF FILE(PRTSTMFF) '
                    +  'DEVTYPE(*USERASCII) '
                    +  'FORMTYPE(' + %trim(formtype) + ') '
                    +  'HOLD(' + %trim(hold) + ') '
                    +  'SAVE(' + %trim(save) + ') '
                    +  'OUTQ(' + %trim(OutQ) + ') '
                    +  'COPIES(' + %editc(copies:'Z') + ')';

         QCMDEXC(cmd: %len(cmd));

         open PRTSTMFF;

         fd = open(stream: O_RDONLY);
         if (fd < 0);
            ReportError();
         endif;

         buf = *allx'20';
         dow (read(fd: %addr(Buf): %size(Buf)) > 0);
            except;
            buf = *allx'20';
         enddo;

         callp close(fd);
         close PRTSTMFF;

         cmd = 'DLTOVR FILE(PRTSTMFF)';
         QCMDEXC(cmd: %len(cmd));

         *inlr = *on;

      /end-free

     OPRTSTMFF  E
     O                       buf                132


      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      * ReportError():  Send an escape message for any errors
      *                 that occurred.
      *
      *  This function requires binding directory QC2LE in order
      *  to access the __errno() function.
      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     P ReportError     B
     D ReportError     PI

     D get_errno       PR              *   ExtProc('__errno')
     D ptrToErrno      s               *
     D errno           s             10I 0 based(ptrToErrno)

     D QMHSNDPM        PR                  ExtPgm('QMHSNDPM')
     D   MessageID                    7A   Const
     D   QualMsgF                    20A   Const
     D   MsgData                  32767A   Const options(*varsize)
     D   MsgDtaLen                   10I 0 Const
     D   MsgType                     10A   Const
     D   CallStkEnt                  10A   Const
     D   CallStkCnt                  10I 0 Const
     D   MessageKey                   4A
     D   ErrorCode                 8192A   options(*varsize)

     D ErrorCode       DS                  qualified
     D  BytesProv              1      4I 0 inz(0)
     D  BytesAvail             5      8I 0 inz(0)

     D MsgKey          S              4A
     D MsgID           s              7A

      /free

         ptrToErrno = get_errno();
         MsgID = 'CPE' + %char(errno);

         QMHSNDPM( MsgID
                 : 'QCPFMSG   *LIBL'
                 : ' '
                 : 0
                 : '*ESCAPE'
                 : '*PGMBDY'
                 : 1
                 : MsgKey
                 : ErrorCode         );

      /end-free
     P                 E

