     h Copyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve notification detail information.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTCOMNOT
     h*  Calls to:
     h*
     h*  Created on 06/11/2014 by C:065 for Project Notifications - WC2277
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     FWMNEP     IF   E             DISK    Rename(PNE01:PNE0P)
     FWDNDL     IF   E           K DISK    Rename(PND01:PND00)
     FWDNDL1    IF   E           K DISK    Rename(PND01:PND01)

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy sPrNotice
      /copy wPrTable
      /copy wPrEmpLr

      * *ENTRY
     DNoteDetail01     pr                  ExtPgm('WTCOMNOT01')
     D                                9  0
     D                            32767
     D                                1
      *
     DNoteDetail01     pi
     D inRRN                          9  0
     D outJSON                    32767
     D rtnCode                        1
      *
     D DocType         s             50    DIM(15) CTDATA PERRCD(1)             Doc Type field names
     D Document        s             50    DIM(15) CTDATA PERRCD(1)             Document field names
     d numEdtCde       c                   const('N')
     d x#Rrn           s              9  0
     d xxKey           s             10
     d xxFold          s            243
     d xxDcid          s             12
     d policyLevel     s              1    inz(' ')
     d x               s              2  0
     d y               s              3  0
     d fold            s              1    DIM(255)
     D psds          esds                  ExtName(szq1p)
     D*** DATA STRUCTURES FOR DARTS REMARK
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
     d
     C*
     C* Notification Detail File Key List
     C*
     C     keyWDNDL      Klist
     C                   Kfld                    NECo#
     C                   Kfld                    NEFnd
     C                   Kfld                    NEEmp#
     C                   Kfld                    NEDiv
     C                   Kfld                    NEFyr
     C                   Kfld                    NECase
     C                   Kfld                    NEType
     C*
     C     keyWDNDL1     Klist
     C                   Kfld                    NECo#
     C                   Kfld                    NEFnd
     C                   Kfld                    NEEmp#
     C                   Kfld                    NEFyr
     C                   Kfld                    NECase
     C                   Kfld                    NEType
     C*
      /Free

       Chain(n) inRRN pne0p;

       Clear qcKey;
       qcCo#  = NECo#;
       qcFnd  = NEFnd;
       qcEmp# = NEEmp#;
       qcDiv  = NEDiv;
       qcQte  = ' ';

       policyLevel = PolicyLevCheck(NEType);

       outJSON = ' ';

       BuildJSON(outJSON :'DtlOutNoteEmplrName' :
        GetEmpFullName(NEEmp#:NEDiv:x#RRN:NEFnd:NECo#:'DBA'));
       BuildJSON(outJSON :'DtlOutNoteFederalName' :
        GetEmpFullName(NEEmp#:NEDiv:x#RRN:NEFnd:NECo#:'FED'));

       x = 1;

       If policyLevel = 'Y';
          Setll keyWDNDL1 pnd01;
          Reade(n) keyWDNDL1 pnd01;
       Else;
          Setll keyWDNDL pnd00;
          Reade(n) keyWDNDL pnd00;
       Endif;

       Dow x <= 15;
          If not %eof;
             BuildJSON(outJSON :DocType(x) : NDKey);
             xxFold = NDFold;
             xxDcid = NDDcid;
             If NDUNCN <> *blanks;
                Exsr movearr;
                y = 255;
                Dow y > 1;
                  If fold(y) = '/';
                     xxfold = %trim(%subst(NDUncn:1:y-1));
                     xxDcid = %trim(%subst(NDUncn:y+1));
                     Leave;
                  Endif;
                  y = y - 1;
                Enddo;
             Endif;

             outJSON = %Trim(outJSON) + ', "' + %trim(Document(x)) +'":';
             StartJSONelemnt(outJSON);                             // Start the element build
             BuildJSON(outJSON :'key' : qcKey );
             BuildJSON(outJSON :'seq' : %Editc(NDSeq#:'Z') );
             BuildJSON(outJSON :'doc_id' : xxDcid );
             BuildJSON(outJSON :'folder' : %trim(xxFold) + '^P' );
             BuildJSON(outJSON :'user' : Q1User );
             BuildJSON(outJSON :'text' : xxDcid );
             EndJSONelemnt(outJSON);                               // End the element build

          Else;
             BuildJSON(outJSON :DocType(x) : ' ');
             BuildJSON(outJSON :Document(x) : ' ');
          Endif;

          x = x + 1;

          If policyLevel = 'Y';
             Reade(n) keyWDNDL1 pnd01;
          Else;
             Reade(n) keyWDNDL pnd00;
          Endif;
       Enddo;

       rtnCode = ' ';
       Return;
      /End-Free
     C**************************************************************************************
     C     movearr       BEGSR
     C                   Movea     NDUncn        fold
     C                   ENDSR                                                  movearr subr
     C**************************************************************************************

** Document Type field names
DtlOutNoteDocType01
DtlOutNoteDocType02
DtlOutNoteDocType03
DtlOutNoteDocType04
DtlOutNoteDocType05
DtlOutNoteDocType06
DtlOutNoteDocType07
DtlOutNoteDocType08
DtlOutNoteDocType09
DtlOutNoteDocType10
DtlOutNoteDocType11
DtlOutNoteDocType12
DtlOutNoteDocType13
DtlOutNoteDocType14
DtlOutNoteDocType15
** Document field names
DtlOutNoteDocument01
DtlOutNoteDocument02
DtlOutNoteDocument03
DtlOutNoteDocument04
DtlOutNoteDocument05
DtlOutNoteDocument06
DtlOutNoteDocument07
DtlOutNoteDocument08
DtlOutNoteDocument09
DtlOutNoteDocument10
DtlOutNoteDocument11
DtlOutNoteDocument12
DtlOutNoteDocument13
DtlOutNoteDocument14
DtlOutNoteDocument15
