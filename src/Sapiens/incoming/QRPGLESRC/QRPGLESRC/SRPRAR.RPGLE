     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H bnddir ('WTBNDDIR')
     H*****************************************************************
     H/TITLE SRPRAR - Check for Prior A/R
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  THIS PROGRAM WILL CHECK FOR PRIOR A/R ON A POLICY TO         *
     H*  DETERMINE IF A ESTIMATED BILL SHOULD BE ALLOWED.             *
     H*                                                               *
     H*  CALLED BY: W8002C                                            *
     H*                                                               *
     H*  CALLS TO:  NONE                                              *
     H*                                                               *
     H*  PROGRAMMER: C:65                                             *
     H*  CREATED:    05/04/15          PROJECT: PHW#33870             *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FWABLL     UF   E           K DISK    RENAME(PBL01:PBL00)
     FWABLL1    IF   E           K DISK    RENAME(PBL01:PBL01)
     FWMEAL10   IF   E           K DISK    RENAME(PEA01:PEA10)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     C*
     C* KEY LIST FOR BILLING REQUEST FILE
     C*
     C     keyWABLL      KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C* KEY LIST FOR A/R FILE
     C*
     C     keyWMEAL10    KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C     *LIKE         DEFINE    BLCO#         XXCO#
     C     *LIKE         DEFINE    BLFND         XXFND
     C     *LIKE         DEFINE    BLFYR         XXFYR
     C     *LIKE         DEFINE    BLEMP#        XXEMP#
     C     *LIKE         DEFINE    EAPODT        XXCURM
     C     *LIKE         DEFINE    BLFLG1        XXPRAR
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURM
     C                   MOVE      '01'          XXCURM
     C*
      /free

       //* Read over all billing request records for the group.
       Setll (xxCo#:xxFnd) pbl01;
       Reade (xxCo#:xxFnd) pbl01;

       Dow not %eof;

          xxPrar = ' ';


          //* If billing calculation or update flags are set process employer.
          If BLFlg1 <> ' ' or BLFlg3 <> ' ';

             xxFyr = BLFyr;
             xxEmp# = BLEmp#;

             Setll keyWMEAL10 pea10;
             Reade keyWMEAL10 pea10;

             //* Check for any a/r posted/transacted prior to current month.
             Dow not %eof and xxPrar = ' ';

                If (EAArty = 'B' and EATrdt <= xxCurm and EAPodt <= xxCurm)
                 or (EAArty = 'A' and EATrdt <= xxCurm and EAPodt <= xxCurm)
                 or (EAArty = 'H' and EATrdt <= xxCurm and EAPodt <= xxCurm)
                 or (EAArty = 'F' and EATrdt <= xxCurm and EAPodt <= xxCurm);
                   xxPrar = 'Y';
                Endif;

                Reade keyWMEAL10 pea10;
             Enddo;

             //* Delete all billing request records for policy year.
             If xxPrar = 'Y';
                Setll keyWABLL pbl00;
                Reade keyWABLL pbl00;

                Dow not %eof;
                   Delete pbl00;
                   Reade keyWABLL pbl00;
                Enddo;
             Endif;

          Endif;

          //* Continue reading billing request records for the group.
          Reade (xxCo#:xxFnd) pbl01;
       Enddo;

       //* End the program.
       *inLR = *on;

      /end-free
