     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE DA700R1 - API TO CALL WRITES A DARTS REMARK FROM VD
     H*****************************************************************
     H* CREATED ON 06/29/2004 BY C:62 FOR PROJECT 129WCS
     H*******************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*   02/28/11  C:220 SUPPORT: 75642
     H*                   CHG TO POPULATE CMFOLD CORRECTLY.           *
     H*                                                               *
     H* 11/02/10 C:183 Project : SY195                                *
     H*                Add two more parms for the path and whether to *
     H*                add root directory (Y/N)                       *
     H*                                                               *

     D**  02 01/18/07 - CHG TO ADD NEW PARM FOR IDARTS COMPATIBILITY  *
     D*
     H*   02 11/15/04  - CHG TO ALLOW SEQ# PASSED BACK TO CALLER      *
     H*                                                               *
     H*******************************************************************
     D*** ADD 1/18/07
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     *** END ADD 1/18/07
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@KEY            40            01
     C                   PARM                    @@COMM          256            02
     C                   PARM                    @@CDAT            8            03 MDCY
     C                   PARM                    @@DCID           12            04
     C                   PARM                    @@PNAM           10            05
     C                   PARM                    @@SECL            3            06
     C                   PARM                    @@USER           10            07
     C                   PARM                    @@PGM            10            08
     C                   PARM                    @@FOLD           80            09
     C                   PARM                    @@DIFL            1            10
     C                   PARM                    @@IPGM           10            11
     C                   PARM                    @@FL01            1            12
     C                   PARM                    @@FL02            1            13
     C                   PARM                    @@RMCD            5            14
     C                   PARM                    @@SEQ#            5            15
     C                   PARM                    @@SBYL            1            16
     C                   PARM                    @@COM2           19            17
     C                   PARM                    @@APID            2            18
     C                   PARM                    @@IDOC           12            19
     C                   PARM                    @@IQUE           10            20
     C                   PARM                    @@IMGF            1            21
     C                   PARM                    @@IPTH           10            22
     C                   PARM                    @@PIDX            5            23
     C                   PARM                    @@QPTY            1            24
     C                   PARM                    @@DCTP            5            25
     C                   PARM                    @@CSID            2            26
     C                   PARM                    @@CODE            2            27
     C                   PARM                    @@VCFN            1            28
     C                   PARM                    @@DPGM           10            29
     C                   PARM                    @@DKEY           40            30
     C** ADD 1/18/07 **** ****************************************
     C                   PARM                    @@IUSR           10            31
     C*** Beg add ***** 11/02/10 **************************************
     C                   PARM                    @@PATH           80            32
     C                   PARM                    @@RPYN            1            33 (Add Root
      *                                                    to Path Y/N)
      *
     C     Q1PRMS        IFGE      32
     C                   MOVEL     @@PATH        X#PATH           80
     C                   ELSE
     C                   MOVEL     *BLANKS       X#PATH
     C                   ENDIF
      *
     C     Q1PRMS        IFGE      33
     C                   MOVEL     @@RPYN        X#RPYN            1
     C                   ELSE
     C                   MOVEL     *BLANKS       X#RPYN
     C                   ENDIF
      *
     C*** End add ***** 11/02/10 **************************************
     C     Q1PRMS        IFGE      31
     C                   MOVEL     @@IUSR        X#IUSR           10             WRKFLD
     ***
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C                   MOVE      XXDT          @@DT              7
     C                   MOVE      DSTIME        @@TM              6
     C*
     C                   ELSE
     C                   MOVEL     *BLANKS       X#IUSR
     C                   MOVE      *BLANKS       @@DT              7
     C                   MOVE      *BLANKS       @@TM              6
     C                   ENDIF
     C**** GET TODAYS DATE TO PASS IN:
     C**
     ***
     ***
     ***
     C**
     C** EJND ADD 1/18/07 **** ****************************************
     C** BEG  ADD 02/28/11 **** ***************************************
     C     @@DIFL        IFEQ      'Y'
     C     @@FOLD        ANDEQ     *BLANKS
     C                   MOVEL     @@KEY         @@FOLD
     C                   ENDIF
     C** END  ADD 02/28/11 **** ***************************************
     C*
     C                   CALL      'DA700R'
     C                   PARM      @@KEY         ##KEY            40            01
     C                   PARM      @@COMM        ##COMM          256            02
     C                   PARM      @@CDAT        ##CDAT            8            03 MDCY
     C                   PARM      @@DCID        ##DCID           12            04
     C                   PARM      @@PNAM        ##PNAM           10            05
     C                   PARM      @@SECL        ##SECL            3            06
     C                   PARM      @@USER        ##USER           10            07
     C                   PARM      @@PGM         ##PGM            10            08
     C                   PARM      @@FOLD        ##FOLD           80            09
     C                   PARM      @@DIFL        ##DIFL            1            10
     C                   PARM      @@IPGM        ##IPGM           10            11
     C                   PARM      @@FL01        ##FL01            1            12
     C                   PARM      @@FL02        ##FL02            1            13
     C                   PARM      @@RMCD        ##RMCD            5            14
     C***DEL 11/15/04 **   PARM @@SEQ#    ##SEQ#  5        15
     C                   PARM                    @@SEQ#                         15
     C                   PARM      @@SBYL        ##SBYL            1            16
     C                   PARM      @@COM2        ##COM2           19            17
     C                   PARM      @@APID        ##APID            2            18
     C                   PARM      @@IDOC        ##IDOC           12            19
     C                   PARM      @@IQUE        ##IQUE           10            20
     C                   PARM      @@IMGF        ##IMGF            1            21
     C                   PARM      @@IPTH        ##IPTH           10            22
     C                   PARM      @@PIDX        ##PIDX            5            23
     C                   PARM      @@QPTY        ##QPTY            1            24
     C                   PARM      @@DCTP        ##DCTP            5            25
     C                   PARM      @@CSID        ##CSID            2            26
     C                   PARM      @@CODE        ##CODE            2            27
     C*** CHANGE 1/18/07 ************************************
     C                   PARM      @@DT          ##TRDT            7            28
     C                   PARM      @@TM          ##TRTM            6            29
     C*** CHGD *********   PARM *BLANKS   ##TRDT  7        28
     C*** CHGD    ******   PARM *BLANKS   ##TRTM  6        29
     C** END CHANGE 1/18/07 ********************************
     C                   PARM      *BLANKS       ##STAT            1            30
     C*** CHANGE 1/18/07 ************************************
     C                   PARM      X#IUSR        ##IUSR           10            31
     C**** CHG *********   PARM *BLANKS   ##IUSR 10        31
     C** END CHANGE 1/18/07 ********************************
     C                   PARM      *BLANKS       ##DCDT            7            32
     C                   PARM      *BLANKS       ##FL03            1            33
     C                   PARM      *BLANKS       ##FL04            1            34
     C                   PARM      *BLANKS       ##FL05            1            35
     C*** CHANGE 1/18/07 ************************************
     C                   PARM      @@DT          ##IDT             7            36
     C*** CHGD *********** PARM *BLANKS   ##IDT   7        36
     C** END CHANGE 1/18/07 ********************************
     C                   PARM      @@VCFN        ##VCFN            1            37
     C                   PARM      @@DPGM        ##DPGM           10            38
     C                   PARM      @@DKEY        ##DKEY           40            39
     C*** Beg add ***** 11/02/10 **************************************
     C                   PARM      X#PATH        ##PATH           80            40
     C                   PARM      X#RPYN        ##RPYN            1            41 (Add Root
      *                                                    to Path Y/N)
     C*** End add ***** 11/02/10 **************************************
     C*
     C                   RETURN
