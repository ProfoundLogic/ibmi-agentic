     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    THIS PROGRAM IS THE COPY/TRANSFER PROGRAM FOR THE DARTS    *
     H*    SINGLE REMARK COPY/TRANSFER.   VDART                       *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    SC915C - DARTS SINGLE REMARK COPY/TRANSFER OVERRIDE        *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  PROGRAMMER: 02                                               *
     H*  CREATED:    05/01/98             PROJECT: 48WHS              *
     H*****************************************************************
     H* FILE SPECIFICATIONS                                           *
     H* SDCMLA:  REMARK DETAIL FILE (KEY/SEQ#/STAT NE 'I')           **
     H* SMARL:   ADDITIONAL REMARKS FILE (KEY/SEQ#/ASQ#/STAT NE I)   **
     H*****************************************************************
    H* REVISIONS:                                                        *
    H*                                                                   *
     H* 08/05/15 C:307 Project: SY354                                     *
     H*                -Adding 'Route To:'                                *
     H*                                                                   *
     H* 08/26/14 C:298 Project: SY354                                     *
     H*                -Implement Routing Page Changes.                   *
     H*                                                                   *
    H* 01/25/13  C:33  Project: SY266                                    *
    H*                 Remove call to SCDMSGR - Hasn't done anything.    *
     H*
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  04/24/07 C:062 PROJECT DR214
     H*                 ADD PROCESSING FOR SMCLP
     H*
     H*  11/29/04 C:062 PROJECT 129WCS
     H*                 DELETE SDAIP
     H*
     H*  07/25/04 C:002 CHANGES TO MAKE SURE CMFL05 IS PASSED TO
     H*                 DA700R SO IT IS COPIED.
     H*
     H*  06/18/03 C:128
     H*                 Added the user who transfered or copied this
     H*                 remark to the additional remarks written when
     H*                 the remark is transfered or copied.
     H*
     H*  04/04/03 C:002
     H*                 Changed TOSEQ# FOR IS901R AND DT WRITE.
     H*
     H*  12/03/02 C:128
     H*                 Changed pgm to use SDCMLE (which includes
     H*                 inactive records) to prevent duplicate keys.
     H*
     H*  05/30/02 C:128 Project:
     H*                 Added logic to call IS901R if this is a
     H*                 transfer. The logic to handle a copy was
     H*                 already in place.                             *
     H*
     H*
     H*  05/22/02 C:128 Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SDCMP with API DA700R
     H*                 2. Missed in original project.(04/25/00)      *
     H*
     H* 05/17/01 C:117 SUPPORT: DR095 SEQ#: 26286                     *
     H*                PUTTING CMSECL AND CMSBYL IN HOLD FIELDS       *
     H*                                                               *
     H*  01/08/01 C:128 Project: DR131
     H*                 Visual Darts issue
     H*                 1. The image flag in SDCMP had a new type added
     H*                    to it. A 'W' = a PC document. New logic was
     H*                    was added to this program to include this
     H*                    type.
     H*  01/04/00 C:113 PROJECT: DR092                                *
     H*                 Added logic to copy/transfer the VDARTS       *
     H*                 multiple object file records, if any.         *
     H*                                                               *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*                                                               *
     H*  01/16/97 002   - PROJ: SUPP                                  *
     H*                   MODIFICATION TO NOT TRANSFER ANY DARTS      *
     H*                   CUSTOM GROUP ID INFORMATION WHEN A SINGLE   *
     H*                   REMARK IS TRANSFERRED.                      *
     H*                                                               *
      *
      *  11/19/97 002   - Proj: 48WJF
      *                   1.  ADDED CODE TO CALL SCDMSGR TO SEND A DIARY
      *                       MESSAGE, IF APPROPRIATE.
      *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*  50  DO WHILE                                                 *
     H*  60  GENERAL CHAINING/DO WHILE                                *
     H*****************************************************************
     F*
     F* FILE SPECIFICATIONS
     F*
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     F*** Begin Chg *** 05/22/02 **************************************
     F****SDCMLA  UF  E           K        DISK                      A
     F*** Begin Chg *** 12/03/02 **************************************
     F****SDCMLA  UF  E           K        DISK
     F*** End   Chg *** 05/22/02 **************************************
     FSDCMLE    UF   E           K DISK
     F*** End   Chg *** 12/03/02 **************************************
     FSMARL     UF A E           K DISK
     FSMDLL     UF A E           K DISK
     FSMKTL     IF   E           K DISK
     FSXDTP     O  A E           K DISK
     F*** BEGIN ADD *** 11/29/04 **************************************
     FSDAIL     UF   E           K DISK
     F*** END   ADD *** 11/29/04 **************************************
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)

     D*
     D* EXCEPTIONS SPECIFICATIONS
     D*
     D RMRK            S             30    DIM(2) CTDATA PERRCD(1)              REMARK TEXT
     D RMRK1           S             12    DIM(4) CTDATA PERRCD(1)              REMARK TEXT
     D*
     D* INPUT SPECIFICATIONS
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXFRDD                 1      8
     D  XMFRDD                 1      2
     D  XDFRDD                 3      4
     D  XCFRDD                 5      6
     D  XYFRDD                 7      8
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* *LIKE DEFINE SECTION
     C*
     C     *LIKE         DEFINE    CMSEQ#        XHSEQ#
     C     *LIKE         DEFINE    CMSEQ#        XAHSEQ
     C     *LIKE         DEFINE    CMSEQ#        XXFRS#
     C     *LIKE         DEFINE    CMSEQ#        XXTOS#
     C     *LIKE         DEFINE    CMCSID        XHCSID
     C     *LIKE         DEFINE    CMCDAT        XHCDAT
     C     *LIKE         DEFINE    CMCDAT        HHDT
     C     *LIKE         DEFINE    CMCODE        XHCODE
     C     *LIKE         DEFINE    CMPNAM        XHPNAM
     C     *LIKE         DEFINE    CMSECL        XHSECL
     C     *LIKE         DEFINE    CMSBYL        XHSBYL
     C     *LIKE         DEFINE    CMSTAT        XHSTAT
     C     *LIKE         DEFINE    CMRMCD        XHRMCD
     C     *LIKE         DEFINE    ORFIL#        XXPRT
     C*** BEGIN ADD *** 04/24/07 **************************************
     C     *LIKE         DEFINE    CMKEY         ##FKEY
     C     *LIKE         DEFINE    CMSEQ#        ##FSEQ
     C     *LIKE         DEFINE    CMKEY         ##TKEY
     C     *LIKE         DEFINE    CMSEQ#        ##TSEQ
     C     *LIKE         DEFINE    CMSTAT        ##CALL
     C*** END   ADD *** 04/24/07 **************************************
     C                   MOVE      *BLANKS       XXMODE            1
     C                   Z-ADD     *ZEROS        RCNT              3 0
     C                   Z-ADD     *ZEROS        ACNT              3 0
     C*
     C* KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C* USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C* KEY LIST FOR THE OUTPUT ROUTING FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT
     C*
     C* REMPAR DETAIL FILE - FROM KEY
     C*
     C     CMKYFR        KLIST
     C                   KFLD                    QQFRKY
     C                   KFLD                    XXFRS#
     C*
     C* REMARK DETAIL FILE - TO KEY
     C*
     C     CMKYTO        KLIST
     C                   KFLD                    QQTOKY
     C*
     C     CMKYT1        KLIST
     C                   KFLD                    QQTOKY
     C                   KFLD                    XHSEQ#
     C*** BEGIN ADD *** 11/29/04 **************************************
     C*
     C* ADDITIONAL INFO FILE - FROM KEY
     C*
     C     AIKYFR        KLIST
     C                   KFLD                    QQFRKY
     C                   KFLD                    XXFRS#
     C*** END   ADD *** 11/29/04 **************************************
     C*
     C* ADDITIONAL REMARKS FILE - FROM KEY
     C*
     C     ARKYFR        KLIST
     C                   KFLD                    QQFRKY
     C                   KFLD                    XXFRS#
     C*
     C* ADDITIONAL REMARKS FILE - TO KEY
     C*
     C     ARKYTO        KLIST
     C                   KFLD                    QQTOKY
     C                   KFLD                    XXTOS#
     C                   KFLD                    XAHSEQ
     C*
     C* ADDITIONAL DETAIL FROM KEY
     C*
     C     DLKYFR        KLIST
     C                   KFLD                    QQFRKY
     C                   KFLD                    XXFRS#
     C*
     C* DARTS KEY TYPE FILE
     C*
     C     KTKY          KLIST
     C                   KFLD                    QQKTYP
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C* ENTRY PARMS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQFRKY           40
     C                   PARM                    QQFRS#            5
     C                   PARM                    QQFRRC            5
     C                   PARM                    QQFRDI            1
     C                   PARM                    QQFRDU           10
     C                   PARM                    QQFRDD            8
     C                   PARM                    QQTOKY           40
     C                   PARM                    QQTORC            5
     C                   PARM                    QQTODI            1
     C                   PARM                    QQTODU           10
     C                   PARM                    QQTODD            8
     C                   PARM                    QQKTYP            2
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C*
     C* EXECUTE THE INITALIZATION SUBROUTINE
     C*
     C                   EXSR      INIT
     C*
     C* DETERMINE IF THE REQUEST IS A TRANSFER OR A COPY OF THE
     C* SELECTED REMARK, EXECUTE THE PROPER SUBROUTINE
     C*
     C     QQTYPE        IFEQ      'C'
     C*
     C                   MOVE      *ON           *IN98
     C                   EXSR      CPYRMK
     C*
     C                   ELSE                                                   ELSE QQTYPE RQ 'C'
     C*
     C                   MOVE      *OFF          *IN98
     C                   EXSR      TFRRMK
     C*
     C                   ENDIF                                                  END QQTYPE EQ 'C'
     C*
     C* EXECUTE THE SUBROUTINE TO WRITE AN ADDITIONAL REMARK FOR THE
     C* NEW 'TO' KEY, INDICATING THIS REMARK WAS COPIED OR TRANSFERRED
     C* FROM THE 'FROM' KEY.
     C*
     C                   EXSR      WRTRMK
     C*
     C* END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C/EJECT
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*******************************************************************
     C* COPY REMARK SUBROUTINE                               C P Y R M K*
     C*******************************************************************
     C     CPYRMK        BEGSR
     C*
     C* IF THE 'TO' MAJOR REMARK EXISTS, WRITE ALL 'FROM' MAJOR REMARKS
     C* TO THE 'TO' MAJOR REMARKS STARTING WITH THE NEXT AVAILABLE
     C* SEQUENCE NUMBER
     C*
     C     CMKYTO        CHAIN(N)  PCM01                              60
     C     *IN60         IFEQ      '0'
     C*** Begin Chg *** 05/22/02 **************************************
     C*
     C* READ THE DARTS FILE TO GET THE NEXT AVAILABLE SEQ# FOR THE
     C* 'TO' KEY AND STORE IN XHSEQ#
     C*
     C******     CMKYT1    SETGTPCM01
     C                   READP(N)  PCM01                                  60
     C******     *IN60     IFEQ '0'
     C******     QQTOKY    ANDEQCMKEY
     C*
     C******               Z-ADD*ZEROS    XHSEQ#
     C******     CMSEQ#    ADD  1         XHSEQ#
     C******               Z-ADDXHSEQ#    XXTOS#
     C*
     C******               ELSE                            ELSE *IN60 EQ '0'
     C*
     C******               Z-ADD*ZEROS    XHSEQ#
     C******               Z-ADDXHSEQ#    XXTOS#
     C*
     C******               END                             END *IN60 EQ '0'
     C*
     C* CHAIN TO THE DARTS MASTER FILE, 'FROM' KEY
     C*
     C     CMKYFR        CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C* Copy the selected Darts major remark from the 'FROM' key
     C* to the new 'TO' key.
     C*
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  QQTOKY        Q#KEY            40
     C                   MOVEL(P)  CMCOMM        Q#COMM          256
     C                   MOVE      CMCOMM        Q#COM2           19
     C     QQTODD        IFNE      *ZEROS
     C                   MOVE      QQTODD        Q#CDAT
     C                   ELSE
     C                   MOVE      CMCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C                   ENDIF                                                  END QQTODD NE *BLANK
     C                   MOVEL(P)  CMDCID        Q#DCID           12
     C                   MOVEL(P)  KTPNAM        Q#PNAM           10
     C                   MOVE      CMSECL        Q#SECL            3
     C     QQTODU        IFNE      *BLANKS
     C                   MOVEL(P)  QQTODU        Q#USER           10
     C                   ELSE                                                   DUSR <> blanks
     C                   MOVEL     CMUSER        Q#USER
     C                   ENDIF                                                  DUSR <> blanks
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  CMFOLD        Q#FOLD           80
     C                   MOVE(P)   QQTODI        Q#DIFL            1
     C                   MOVEL(P)  CMIPGM        Q#IPGM           10
     C                   MOVEL(P)  CMFL01        Q#FL01            1
     C                   MOVEL(P)  CMFL02        Q#FL02            1
     C                   MOVEL(P)  QQTORC        Q#RMCD            5
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  CMSBYL        Q#SBYL            1
     C                   MOVEL(P)  CMAPID        Q#APID            2
     C                   MOVEL(P)  CMIDOC        Q#IDOC           12
     C                   MOVEL(P)  CMIQUE        Q#IQUE           10
     C                   MOVEL(P)  CMIMGF        Q#IMGF            1
     C                   MOVEL(P)  CMIPTH        Q#IPTH           10
     C                   MOVE      CMPIDX        Q#PIDX            5
     C                   MOVEL(P)  CMQPTY        Q#QPTY            1
     C                   MOVEL(P)  CMDCTP        Q#DCTP            5
     C                   MOVEL(P)  CMCSID        Q#CSID            2
     C                   MOVEL(P)  CMCODE        Q#CODE            2
     C**** ADD ADDITIONAL FIELDS FOR DA700R PROCESSING  07/25/04 ********
     C                   MOVE      CMTRDT        Q#TRDT            7
     C                   MOVE      CMTRTM        Q#TRTM            6
     C                   MOVE      CMSTAT        Q#STAT            1
     C                   MOVE      CMIUSR        Q#IUSR           10
     C                   MOVE      CMDCDT        Q#DCDT            7
     C                   MOVE      CMFL03        Q#FL03            1
     C                   MOVE      CMFL04        Q#FL04            1
     C                   MOVE      CMFL05        Q#FL05            1
     C                   MOVE      CMIDT         Q#IDT             7
     C***** END ADD 07/25/04 ***************************************************
     C*** BEGIN ADD *** 11/29/04 **************************************
     C     ##DEIF        IFEQ      'Y'
     C     AIKYFR        CHAIN(N)  PAI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     AIVCFN        Q#VCFN
     C                   MOVEL     AIDPGM        Q#DPGM
     C     AIDPGM        IFNE      *BLANKS
     C                   MOVEL     QQTOKY        Q#DKEY
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     *BLANKS       Q#VCFN
     C                   MOVEL     '*NO'         Q#DPGM
     C                   MOVEL     *BLANKS       Q#DKEY
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     *BLANKS       Q#VCFN
     C                   MOVEL     '*NO'         Q#DPGM
     C                   MOVEL     *BLANKS       Q#DKEY
     C                   ENDIF
     C*** END   ADD *** 11/29/04 **************************************
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CODE
     C**** ADD ADDITIONAL FIELDS FOR DA700R PROCESSING  07/25/04 ********
     C                   PARM                    Q#TRDT            7
     C                   PARM                    Q#TRTM            6
     C                   PARM                    Q#STAT            1
     C                   PARM                    Q#IUSR           10
     C                   PARM                    Q#DCDT            7
     C                   PARM                    Q#FL03            1
     C                   PARM                    Q#FL04            1
     C                   PARM                    Q#FL05            1
     C                   PARM                    Q#IDT             7
     C***** END ADD 07/25/04 ***************************************************
     C*** BEGIN ADD *** 11/29/04 **************************************
     C                   PARM                    Q#VCFN            1
     C                   PARM                    Q#DPGM           10
     C                   PARM                    Q#DKEY           40
     C*** END   ADD *** 11/29/04 **************************************
     C*** BEGIN ADD *** 04/24/07 **************************************
     C*
     C* COPY SMCLP
     C*
     C                   MOVE      QQFRS#        ##FSEQ
     C                   MOVE      Q#SEQ#        ##TSEQ
     C*
     C                   CALL      'DA703R'
     C                   PARM      QQFRKY        ##FKEY
     C                   PARM                    ##FSEQ
     C                   PARM      QQTOKY        ##TKEY
     C                   PARM                    ##TSEQ
     C                   PARM      'C'           ##CALL
     C*** END   ADD *** 04/24/07 **************************************
      *
     C                   MOVE      Q#SEQ#        XHSEQ#
     C                   MOVE      Q#SEQ#        XXTOS#
     C*** End Chg *** 05/22/02 ****************************************
     C*
     C* MOVE DARTS FIELDS TO HOLD FIELDS FOR USE LATER IN THE
     C* SUBROUTINE THAT WILL WRITE ADDITIONAL REMARKS
     C*
     C                   MOVE      CMCSID        XHCSID
     C                   Z-ADD     CMCDAT        XHCDAT
     C                   MOVE      CMCODE        XHCODE
     C                   MOVE      CMPNAM        XHPNAM
     C                   Z-ADD     CMSECL        XHSECL
     C                   MOVE      CMSBYL        XHSBYL
     C                   MOVE      CMSTAT        XHSTAT
     C*
     C* COPY ALL DARTS ADDITIONAL REMARKS ASSOCIATED WITH THIS
     C* 'FROM' MAJOR REMARK
     C*
     C     ARKYFR        CHAIN     PAR01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C                   MOVEL     QQTOKY        ARKEY
     C                   MOVEL     XHSEQ#        ARSEQ#
     C                   MOVEL     Q1PGM         ARPGM
     C                   MOVEL     KTPNAM        ARPNAM
     C                   MOVE      DSTIME        ARTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HHDT
     C                   Z-ADD     HHDT          ARDT
     C                   ADD       1             ACNT
     C                   WRITE     PAR01
     C*
     C     ARKYFR        READE     PAR01                                  50
     C                   ENDDO                                                  END *IN50 DOWEQ '0'
     C*
     C* COPY ALL DARTS CUSTOM GROUP ID RECORDS ASSOCIATED WITH
     C* THIS 'FROM' MAJOR REMARK
     C*
     C     CMFL05        IFEQ      'O'
     C*
     C     DLKYFR        CHAIN     PDL01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C                   MOVEL     QQTOKY        DLKEY
     C                   MOVEL     XHSEQ#        DLSEQ#
     C                   MOVEL     Q1PGM         DLPGM
     C                   MOVE      DSTIME        DLTIME
     C     DSDATE        MULT      10000.01      DLDT
     C                   WRITE     PDL01
     C*
     C     DLKYFR        READE     PDL01                                  50
     C                   ENDDO                                                   END *IN50 DOWEQ '0'
     C*
     C                   ENDIF                                                   END CMFL05 EQ 'O'
     C*** BEGIN ADD *** 01/04/00 ***********************************************
     C*
     C* Call program to copy the Multiple-Object records, if any.
     C*
     C     CMQPTY        IFEQ      'Y'
     C*
     C                   MOVE      CMSEQ#        QQFSEQ            5
     C                   MOVE      XHSEQ#        QQTSEQ            5
     C                   MOVE      'C'           QXMODE            1
     C*
     C                   CALL      'SC917R'
     C                   PARM                    QQFRKY
     C                   PARM                    QQFSEQ
     C                   PARM                    QQTOKY
     C                   PARM                    QQTSEQ
     C                   PARM                    QXMODE
     C*
     C                   ENDIF
     C*** END   ADD *** 01/04/00 ***********************************************
     C*** Begin Chg *** 05/22/02 **************************************
     C*
     C* COPY THE SELECTED DARTS MAJOR REMARK FROM THE 'FROM' KEY
     C* TO THE NEW 'TO' KEY
     C*
     C******               MOVELQQTOKY    CMKEY
     C******               MOVELXHSEQ#    CMSEQ#
     C******               MOVE QQTORC    CMRMCD
     C******               MOVE QQTODI    CMDIFL
     C*
     C******     QQTODU    IFNE *BLANKS
     C*
     C******               MOVE QQTODU    CMUSER
     C*
     C******               ENDIF                           END QQTODU NE *BLANKS
     C*
     C******     QQTODD    IFNE *ZEROS
     C*
     C******               MOVE QQTODD    SRDTI
     C******               CALL 'SRCVTDAT'DTCYMD
     C******               MOVELSRDTO     CMCDAT
     C*
     C******               ENDIF                           END QQTODD NE *BLANKS
     C*
     C******               MOVELKTPNAM    CMPNAM
     C******               MOVELQ1PGM     CMPGM
     C******               MOVE DSTIME    CMTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HHDT
     C                   Z-ADD     HHDT          CMDT
     C                   ADD       1             RCNT
     C******               WRITEPCM01
     C***** BEGIN ADD **** 07/01/99 ***********************************
     C******               FEOD SDCMLA
     C***** END   ADD **** 07/01/99 ***********************************
     C*** End   Chg *** 05/22/02 **************************************
      *
     C**** ADD 11/19/97 *** START ************ 48WJF ********************
     C** CALL PROGRAM TO PROCESS THE SEND OF DIARY MESSAGE
     C     CMUSER        IFNE      Q1USER
     C     CMDIFL        ANDEQ     'Y'
     C*
     C***  Begin Del  ***  01/25/13  **************************************
     C**********         MOVE      XHCDAT        #MTDDT
     C**********         MOVE      CMSEQ#        #MSEQ#
     C**********         CALL      'SCDMSGR '                           60
     C**********         PARM      CMKEY         #MKEY            40
     C**********         PARM                    #MSEQ#            5
     C**********         PARM      CMUSER        #MTUSR           10
     C**********         PARM                    #MTDDT            6
     C**********         PARM      CMRMCD        #MTRMC            5
     C**********         PARM      CMPNAM        #MTPGM           10
     C**********         PARM      *BLANK        #MRTC             1
     C***  End   Del  ***  01/25/13  **************************************
     C*
     C                   ENDIF
     C*
     C**** ADD 11/19/97 *** END ************** 48WJF ********************
     C*
     C*
     C                   ENDIF                                                  END *IN60 EQ '0'
     C*
     C* ELSE 'TO' MAJOR REMARK DOES NOT EXIST, SO ALL 'FROM' REMARKS
     C* ADDITIONAL REMARKS, AND CUSTOM GROUP ID CAN BE UPDATED WITH
     C* THE 'TO' KEY VALUES
     C*
     C                   ELSE                                                   ELSE *IN60 EQ '1'
     C*
     C**** Begin Chg 05/22/02 *******************************************
     C******               Z-ADD*ZEROS    XHSEQ#
     C******               Z-ADDXHSEQ#    XXTOS#
     C**** End   Chg 05/22/02 *******************************************
     C     CMKYFR        CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C**** Begin Chg 05/22/02 *******************************************
     C*
     C* MOVE DARTS FIELDS TO HOLD FIELDS FOR USE LATER IN THE
     C* SUBROUTINE THAT WILL WRITE ADDITIONAL REMARKS
     C*
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  QQTOKY        Q#KEY            40
     C                   MOVEL(P)  CMCOMM        Q#COMM          256
     C                   MOVE      CMCOMM        Q#COM2           19
     C     QQTODD        IFNE      *ZEROS
     C                   MOVE      QQTODD        Q#CDAT
     C                   ELSE
     C                   MOVE      CMCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C                   ENDIF                                                  END QQTODD NE *BLANK
     C                   MOVEL(P)  CMDCID        Q#DCID           12
     C                   MOVEL(P)  KTPNAM        Q#PNAM           10
     C                   MOVE      CMSECL        Q#SECL            3
     C     QQTODU        IFNE      *BLANKS
     C                   MOVEL(P)  QQTODU        Q#USER           10
     C                   ELSE                                                   DUSR <> blanks
     C                   MOVEL     CMUSER        Q#USER
     C                   ENDIF                                                  DUSR <> blanks
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  CMFOLD        Q#FOLD           80
     C                   MOVE(P)   QQTODI        Q#DIFL            1
     C                   MOVEL(P)  CMIPGM        Q#IPGM           10
     C                   MOVEL(P)  CMFL01        Q#FL01            1
     C                   MOVEL(P)  CMFL02        Q#FL02            1
     C                   MOVEL(P)  QQTORC        Q#RMCD            5
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  CMSBYL        Q#SBYL            1
     C                   MOVEL(P)  CMAPID        Q#APID            2
     C                   MOVEL(P)  CMIDOC        Q#IDOC           12
     C                   MOVEL(P)  CMIQUE        Q#IQUE           10
     C                   MOVEL(P)  CMIMGF        Q#IMGF            1
     C                   MOVEL(P)  CMIPTH        Q#IPTH           10
     C                   MOVE      CMPIDX        Q#PIDX            5
     C                   MOVEL(P)  CMQPTY        Q#QPTY            1
     C                   MOVEL(P)  CMDCTP        Q#DCTP            5
     C                   MOVEL(P)  CMCSID        Q#CSID            2
     C                   MOVEL(P)  CMCODE        Q#CODE            2
     C**** ADD ADDITIONAL FIELDS FOR DA700R PROCESSING  07/25/04 ********
     C                   MOVE      CMTRDT        Q#TRDT            7
     C                   MOVE      CMTRTM        Q#TRTM            6
     C                   MOVE      CMSTAT        Q#STAT            1
     C                   MOVE      CMIUSR        Q#IUSR           10
     C                   MOVE      CMDCDT        Q#DCDT            7
     C                   MOVE      CMFL03        Q#FL03            1
     C                   MOVE      CMFL04        Q#FL04            1
     C                   MOVE      CMFL05        Q#FL05            1
     C                   MOVE      CMIDT         Q#IDT             7
     C*** BEGIN ADD *** 11/29/04 **************************************
     C     ##DEIF        IFEQ      'Y'
     C     AIKYFR        CHAIN(N)  PAI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     AIVCFN        Q#VCFN
     C                   MOVEL     AIDPGM        Q#DPGM
     C     AIDPGM        IFNE      *BLANKS
     C                   MOVEL     QQTOKY        Q#DKEY
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     *BLANKS       Q#VCFN
     C                   MOVEL     '*NO'         Q#DPGM
     C                   MOVEL     *BLANKS       Q#DKEY
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     *BLANKS       Q#VCFN
     C                   MOVEL     '*NO'         Q#DPGM
     C                   MOVEL     *BLANKS       Q#DKEY
     C                   ENDIF
     C*** END   ADD *** 11/29/04 **************************************
     C***** END ADD 07/25/04 ***************************************************
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CODE
     C**** ADD ADDITIONAL FIELDS FOR DA700R PROCESSING  07/25/04 ********
     C                   PARM                    Q#TRDT            7
     C                   PARM                    Q#TRTM            6
     C                   PARM                    Q#STAT            1
     C                   PARM                    Q#IUSR           10
     C                   PARM                    Q#DCDT            7
     C                   PARM                    Q#FL03            1
     C                   PARM                    Q#FL04            1
     C                   PARM                    Q#FL05            1
     C                   PARM                    Q#IDT             7
     C***** END ADD 07/25/04 ***************************************************
     C*** BEGIN ADD *** 11/29/04 **************************************
     C                   PARM                    Q#VCFN
     C                   PARM                    Q#DPGM
     C                   PARM                    Q#DKEY
     C*** END   ADD *** 11/29/04 **************************************
      *
     C                   MOVE      Q#SEQ#        XHSEQ#
     C                   MOVE      Q#SEQ#        XXTOS#
     C*** End Chg *** 05/22/02 ****************************************
     C*
     C* MOVE DARTS FIELDS TO HOLD FIELDS FOR USE LATER IN THE
     C* SUBROUTINE THAT WILL WRITE ADDITIONAL REMARKS
     C*
     C                   MOVE      CMCSID        XHCSID
     C                   Z-ADD     CMCDAT        XHCDAT
     C                   MOVE      CMCODE        XHCODE
     C                   MOVE      CMPNAM        XHPNAM
     C                   Z-ADD     CMSECL        XHSECL
     C                   MOVE      CMSBYL        XHSBYL
     C                   MOVE      CMSTAT        XHSTAT
     C*
     C* COPY ALL DARTS ADDITIONAL REMARKS ASSOCIATED WITH THIS
     C* 'FROM' MAJOR REMARK
     C*
     C     ARKYFR        CHAIN     PAR01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C                   MOVEL     QQTOKY        ARKEY
     C                   Z-ADD     XHSEQ#        ARSEQ#
     C                   MOVEL     KTPNAM        ARPNAM
     C                   MOVEL     Q1PGM         ARPGM
     C                   MOVE      DSTIME        ARTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HHDT
     C                   Z-ADD     HHDT          ARDT
     C                   ADD       1             ACNT
     C                   WRITE     PAR01
     C*
     C     ARKYFR        READE     PAR01                                  50
     C                   ENDDO                                                  END *IN50 DOWEQ '0'
     C*
     C* COPY ALL DARTS CUSTOM GROUP IDS ASSOCIATED WITH THIS
     C* 'FROM' MAJOR REMARK
     C*
     C     CMFL05        IFEQ      'O'
     C*
     C     DLKYFR        CHAIN     PDL01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C                   MOVEL     QQTOKY        DLKEY
     C                   Z-ADD     XHSEQ#        DLSEQ#
     C                   MOVEL     Q1PGM         DLPGM
     C                   MOVE      DSTIME        DLTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HHDT
     C                   Z-ADD     HHDT          DLDT
     C                   WRITE     PDL01
     C*
     C     DLKYFR        READE     PDL01                                  50
     C                   ENDDO                                                   END *IN50 DOWEQ '0'
     C*
     C                   ENDIF                                                   END CMFL05 EQ 'O'
     C*** BEGIN ADD *** 01/04/00 ***********************************************
     C*
     C* Call program to copy the Multiple-Object records, if any.
     C*
     C     CMQPTY        IFEQ      'Y'
     C*
     C                   MOVE      CMSEQ#        QQFSEQ            5
     C                   MOVE      XHSEQ#        QQTSEQ            5
     C                   MOVE      'C'           QXMODE            1
     C*
     C                   CALL      'SC917R'
     C                   PARM                    QQFRKY
     C                   PARM                    QQFSEQ
     C                   PARM                    QQTOKY
     C                   PARM                    QQTSEQ
     C                   PARM                    QXMODE
     C*
     C                   ENDIF
     C*** END   ADD *** 01/04/00 ***********************************************
     C*** Begin Chg * 05/22/02 ****************************************
     C*
     C* COPY THE SELECTED DARTS MAJOR REMARK FROM THE 'FROM' KEY
     C* TO THE NEW 'TO' KEY
     C*
     C******               MOVELQQTOKY    CMKEY
     C******               Z-ADDXXTOS#    CMSEQ#
     C******               MOVE QQTORC    CMRMCD
     C******               MOVE QQTODI    CMDIFL
     C*
     C******     QQTODU    IFNE *BLANKS
     C*
     C******               MOVE QQTODU    CMUSER
     C*
     C******               ENDIF                           END QQTODU NE *BLANKS
     C*
     C******     QQTODD    IFNE *ZEROS
     C*
     C******               MOVE QQTODD    SRDTI
     C******               CALL 'SRCVTDAT'DTCYMD
     C******               MOVELSRDTO     CMCDAT
     C*
     C******               ENDIF                           END QQTODD NE *BLANKS
     C*
     C******               MOVELKTPNAM    CMPNAM
     C******               MOVELQ1PGM     CMPGM
     C                   MOVE      DSTIME        CMTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HHDT
     C                   Z-ADD     HHDT          CMDT
     C                   ADD       1             RCNT
     C******               WRITEPCM01
     C***** BEGIN ADD **** 07/01/99 ***********************************
     C******               FEOD SDCMLA
     C***** END   ADD **** 07/01/99 ***********************************
     C*** End   Chg * 05/22/02 ****************************************
      *
     C**** ADD 11/19/97 *** START ************ 48WJF ********************
     C** CALL PROGRAM TO PROCESS THE SEND OF DIARY MESSAGE
     C     CMUSER        IFNE      Q1USER
     C     CMDIFL        ANDEQ     'Y'
     C*
     C***  Begin Del  ***  01/25/13  **************************************
     C**********         MOVE      XHCDAT        #MTDDT
     C**********         MOVE      CMSEQ#        #MSEQ#
     C**********         CALL      'SCDMSGR '                           60
     C**********         PARM      CMKEY         #MKEY            40
     C**********         PARM                    #MSEQ#            5
     C**********         PARM      CMUSER        #MTUSR           10
     C**********         PARM                    #MTDDT            6
     C**********         PARM      CMRMCD        #MTRMC            5
     C**********         PARM      CMPNAM        #MTPGM           10
     C**********         PARM      *BLANK        #MRTC             1
     C***  End   Del  ***  01/25/13  **************************************
     C*
     C                   ENDIF
     C*
     C**** ADD 11/19/97 *** END ************** 48WJF ********************
     C*
     C                   ENDIF                                                  END *IN60 EQ '0'
     C*
     C                   ENDIF                                                  END *IN60 EQ '0'
     C*
     C* PRINT OUT THE 'FROM' AND 'TO' KEYS, AND THE ADDITIONAL REMARK
     C* TOTALS, FOR THIS TRANSFER
     C*
     C                   EXCEPT    RMK
     C                   MOVE      QQFRKY        DTFRKY
     C                   MOVE      QQFRS#        DTFRS#
     C                   MOVE      QQFRRC        DTFRRC
     C                   MOVE      QQFRDI        DTFRDI
     C                   MOVE      QQFRDU        DTFRDU
     C                   MOVE      QQFRDD        DTFRDD
     C                   MOVE      QQTOKY        DTTOKY
     C*** CHG 04/04/03 *** MOVE CMSEQ#    DTTOS#
     C                   MOVE      XHSEQ#        DTTOS#
     C                   MOVE      QQTORC        DTTORC
     C                   MOVE      QQTODI        DTTODI
     C                   MOVE      QQTODD        DTTODD
     C                   MOVE      QQTODU        DTTODU
     C                   WRITE     PDT01
     C     CMIMGF        IFNE      ' '
     C*** BEGIN ADD *** 01/08/01 ***********************************************
     C     CMIMGF        ANDNE     'W'
     C*** END   ADD *** 01/08/01 ***********************************************
     C                   MOVE      QQFRS#        @@FRS#
     C                   CALL      'IS901R'                             90
     C                   PARM      CMIDOC        @@IDOC           12
     C                   PARM      CMFOLD        @@FOLD           80
     C                   PARM      QQFRKY        @@FKY            40
     C                   PARM                    @@FRS#            5 0
     C                   PARM      QQTOKY        @@TKY            40
     C*** CHG 04/04/03 *** PARM CMSEQ#    @@TOS#  50
     C                   PARM      XHSEQ#        @@TOS#            5 0
     C                   PARM      QQTYPE        @@TYPE            1
     C                   ENDIF
     C*
     C                   ENDSR
     C/EJECT
     C*******************************************************************
     C* INITALIZATION SUBROUTINE                                 I N I T*
     C*******************************************************************
     C     INIT          BEGSR
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      QQMODE        XXMODE
     C                   MOVE      QQFRDD        XXFRDD
     C                   MOVE      QQFRS#        XXFRS#
     C                   Z-ADD     *ZEROS        RCNT
     C                   Z-ADD     *ZEROS        ACNT
     C                   Z-ADD     *HIVAL        XHSEQ#
     C                   Z-ADD     *HIVAL        XAHSEQ
     C                   MOVE      *BLANKS       XHCSID
     C                   Z-ADD     *ZEROS        XHCDAT
     C                   MOVE      *BLANKS       XHCODE
     C                   MOVE      *BLANKS       XHPNAM
     C                   Z-ADD     *ZEROS        XHSECL
     C                   MOVE      *BLANKS       XHSBYL
     C                   MOVE      *BLANKS       XHSTAT
     C                   MOVE      *BLANKS       XHRMCD
     C                   MOVE      QQFRDD        @@FRDD            8 0
     C                   MOVE      QQFRS#        @@FRS#            5 0
     C                   MOVE      QQTODD        @@TODD            8 0
     C*** BEGIN ADD *** 11/29/04 **************************************
     C                   CALL      'SA001R'
01   C                   PARM      *BLANKS       ##WCIF            1            WC INST
02   C                   PARM      *BLANKS       ##MCIF            1            MC INST
03   C                   PARM      *BLANKS       ##WCIN            1            WC INTERFACE
04   C                   PARM      *BLANKS       ##AR              1            MC AR INST
05   C                   PARM      *BLANKS       ##NTWK            1            NETWORK INST
06   C                   PARM      *BLANKS       ##UR              1            PRECERT INST
07   C                   PARM      *BLANKS       ##MED             1            MEDMGMT INST
08   C                   PARM      *BLANKS       ##EDI             1            EDI INST
09   C                   PARM      *BLANKS       ##WCDK            1            WC DARTS KEY
10   C                   PARM      *BLANKS       ##RHAB            1            REHAB INST
11   C                   PARM      *BLANKS       ##MSS             1            MS SUPPORT
12   C                   PARM      *BLANKS       ##FSCH            1            FEE SCHEDULE
13   C                   PARM      *BLANKS       ##PRVB            1            PROVIDER BILLS
14   C                   PARM      *BLANKS       ##NPWC            1            NON-PDS WC  LS
15   C                   PARM      *BLANKS       ##OS              1            OUTOME/SAVINGS
16   C                   PARM      *BLANKS       ##ARLS            1            Auto Release Flag
17   C                   PARM      *BLANKS       ##CCOD            1            Correct Coding Flag
18   C                   PARM      *BLANKS       ##CRVW            1            Code Review Flag
19   C                   PARM      *BLANKS       ##PROM            1            ProMatch Flag
20   C                   PARM      *BLANKS       ##MCRV            1            Medicare CR Flag
21   C                   PARM      *BLANKS       ##FREQ            1            Frequency Flag
22   C                   PARM      *BLANKS       ##CRAC            1            CR Auto Complete
23   C                   PARM      *BLANKS       ##PREO            1            CR Auto Complete
24   C                   PARM      *BLANKS       ##PARN            1            Par/Non-Par/Both
25   C                   PARM      *BLANKS       ##MGRI            1            Multi Group Restr
26   C                   PARM      *BLANKS       ##KCIF            1            Keep Claimant info
27   C                   PARM      *BLANKS       ##CURS            1            PBILLS entry cursor
28   C                   PARM      *BLANKS       ##PCIB            1            Prov Check in Batch
29   C                   PARM      *BLANKS       ##CLT#            3            Client Number
30   C                   PARM      *BLANKS       ##UCPF            1            UCR Percentile
31   C                   PARM      *BLANKS       ##DCF             1            Data Center
32   C                   PARM      *BLANKS       ##AUTB            1            Auto Batch   Flag
33   C                   PARM      *BLANKS       ##DEIF            1            DE/Image Flag
     C*** END   ADD *** 11/29/04 **************************************
     C*
     C* SET-UP AND INITIALIZE
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C* CHAIN TO THE KEY TYPE FILE
     C*
     C     KTKY          CHAIN     PKT01                              60
     C     *IN60         IFEQ      *ON
     C*
     C                   MOVE      *BLANKS       KTPNAM
     C*
     C                   ENDIF                                                  END *IN60 EQ *ON
     C*
     C* GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C* GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*
     C* GET THE OUTPUT ROUTING INFORMATION, IF SUCESSFUL, PRINT THE
     C* ROUTING PAGE
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C********           MOVE      '0'           XXPRT
     C********     ORKEY         CHAIN     POR01                              60
     C********     *IN60         IFEQ      '0'
     C*
     C********           EXCEPT    ROUTE
     C*
     C********           ENDIF                                                  END *IN60 EQ '0'
      ***** End Del ***** 08/26/14 ************************************
     C*
     C* PRINT PAGE HEADING
     C*
     C                   EXCEPT    HEAD
     C*
     C                   ENDSR
     C/EJECT
     C*******************************************************************
     C* TRANSFER REMARK SUBROUTINE                           T F R R M K*
     C*******************************************************************
     C     TFRRMK        BEGSR
     C*
     C* IF THE 'TO' MAJOR REMARK EXISTS, WRITE ALL 'FROM' MAJOR REMARKS
     C* TO THE 'TO' MAJOR REMARKS STARTING WITH THE NEXT AVAILABLE
     C* SEQUENCE NUMBER
     C*
     C     CMKYTO        CHAIN(N)  PCM01                              60
     C     *IN60         IFEQ      '0'
     C*** Begin Chg *** 05/22/02 **************************************
     C*
     C* READ THE DARTS FILE TO GET THE NEXT AVAILABLE SEQ# FOR THE
     C* 'TO' KEY AND STORE IN XHSEQ#
     C*
     C******     CMKYT1    SETGTPCM01
     C******               READPPCM01               N    60
     C******     *IN60     IFEQ '0'
     C******     QQTOKY    ANDEQCMKEY
     C*
     C******               Z-ADD*ZEROS    XHSEQ#
     C******     CMSEQ#    ADD  1         XHSEQ#
     C******               Z-ADDXHSEQ#    XXTOS#
     C*
     C******               ELSE                            ELSE *IN60 EQ '0'
     C*
     C******               Z-ADD*ZEROS    XHSEQ#
     C******               Z-ADDXHSEQ#    XXTOS#
     C*
     C******               END                             END *IN60 EQ '0'
     C*
     C* CHAIN TO THE DARTS MASTER FILE, 'FROM' KEY
     C*
     C     CMKYFR        CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C* Transfer the selected DARTS major remark from the 'FROM' key
     C* to the new 'TO' key.
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  QQTOKY        Q#KEY            40
     C                   MOVEL(P)  CMCOMM        Q#COMM          256
     C                   MOVE      CMCOMM        Q#COM2           19
     C     QQTODD        IFNE      *ZEROS
     C                   MOVE      QQTODD        Q#CDAT
     C                   ELSE
     C                   MOVE      CMCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C                   ENDIF                                                  END QQTODD NE *BLANK
     C                   MOVEL(P)  CMDCID        Q#DCID           12
     C                   MOVEL(P)  KTPNAM        Q#PNAM           10
     C                   MOVE      CMSECL        Q#SECL            3
     C     QQTODU        IFNE      *BLANKS
     C                   MOVEL(P)  QQTODU        Q#USER           10
     C                   ELSE                                                   DUSR <> blanks
     C                   MOVEL     CMUSER        Q#USER
     C                   ENDIF                                                  DUSR <> blanks
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  CMFOLD        Q#FOLD           80
     C                   MOVE(P)   QQTODI        Q#DIFL            1
     C                   MOVEL(P)  CMIPGM        Q#IPGM           10
     C                   MOVEL(P)  CMFL01        Q#FL01            1
     C                   MOVEL(P)  CMFL02        Q#FL02            1
     C                   MOVEL(P)  QQTORC        Q#RMCD            5
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  CMSBYL        Q#SBYL            1
     C                   MOVEL(P)  CMAPID        Q#APID            2
     C                   MOVEL(P)  CMIDOC        Q#IDOC           12
     C                   MOVEL(P)  CMIQUE        Q#IQUE           10
     C                   MOVEL(P)  CMIMGF        Q#IMGF            1
     C                   MOVEL(P)  CMIPTH        Q#IPTH           10
     C                   MOVE      CMPIDX        Q#PIDX            5
     C                   MOVEL(P)  CMQPTY        Q#QPTY            1
     C                   MOVEL(P)  CMDCTP        Q#DCTP            5
     C                   MOVEL(P)  CMCSID        Q#CSID            2
     C                   MOVEL(P)  CMCODE        Q#CODE            2
     C**** ADD ADDITIONAL FIELDS FOR DA700R PROCESSING  07/25/04 ********
     C                   MOVE      CMTRDT        Q#TRDT            7
     C                   MOVE      CMTRTM        Q#TRTM            6
     C                   MOVE      CMSTAT        Q#STAT            1
     C                   MOVE      CMIUSR        Q#IUSR           10
     C                   MOVE      CMDCDT        Q#DCDT            7
     C                   MOVE      CMFL03        Q#FL03            1
     C                   MOVE      CMFL04        Q#FL04            1
     C                   MOVE      CMFL05        Q#FL05            1
     C                   MOVE      CMIDT         Q#IDT             7
     C*** BEGIN ADD *** 11/29/04 **************************************
     C     ##DEIF        IFEQ      'Y'
     C     AIKYFR        CHAIN(N)  PAI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     AIVCFN        Q#VCFN
     C                   MOVEL     AIDPGM        Q#DPGM
     C     AIDPGM        IFNE      *BLANKS
     C                   MOVEL     QQTOKY        Q#DKEY
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     *BLANKS       Q#VCFN
     C                   MOVEL     '*NO'         Q#DPGM
     C                   MOVEL     *BLANKS       Q#DKEY
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     *BLANKS       Q#VCFN
     C                   MOVEL     '*NO'         Q#DPGM
     C                   MOVEL     *BLANKS       Q#DKEY
     C                   ENDIF
     C*** END   ADD *** 11/29/04 **************************************
     C***** END ADD 07/25/04 ***************************************************
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CODE
     C**** ADD ADDITIONAL FIELDS FOR DA700R PROCESSING  07/25/04 ********
     C                   PARM                    Q#TRDT            7
     C                   PARM                    Q#TRTM            6
     C                   PARM                    Q#STAT            1
     C                   PARM                    Q#IUSR           10
     C                   PARM                    Q#DCDT            7
     C                   PARM                    Q#FL03            1
     C                   PARM                    Q#FL04            1
     C                   PARM                    Q#FL05            1
     C                   PARM                    Q#IDT             7
     C***** END ADD 07/25/04 ***************************************************
     C*** BEGIN ADD *** 11/29/04 **************************************
     C                   PARM                    Q#VCFN
     C                   PARM                    Q#DPGM
     C                   PARM                    Q#DKEY
     C*** END   ADD *** 11/29/04 **************************************
     C*** BEGIN ADD *** 04/24/07 **************************************
     C*
     C* TRANSFER SMCLP
     C*
     C                   MOVE      QQFRS#        ##FSEQ
     C                   MOVE      Q#SEQ#        ##TSEQ
     C*
     C                   CALL      'DA703R'
     C                   PARM      QQFRKY        ##FKEY
     C                   PARM                    ##FSEQ
     C                   PARM      QQTOKY        ##TKEY
     C                   PARM                    ##TSEQ
     C                   PARM      *BLANKS       ##CALL
     C*** END   ADD *** 04/24/07 **************************************
      *
     C                   MOVE      Q#SEQ#        XHSEQ#
     C                   MOVE      Q#SEQ#        XXTOS#
     C*** End Chg *** 05/22/02 ****************************************
     C*
     C* MOVE DARTS FIELDS TO HOLD FIELDS FOR USE LATER IN THE
     C* SUBROUTINE THAT WILL WRITE ADDITIONAL REMARKS
     C*
     C                   MOVE      Q#CSID        XHCSID
     C                   Z-ADD     CMCDAT        XHCDAT
     C                   MOVE      Q#CODE        XHCODE
     C                   MOVE      Q#PNAM        XHPNAM
     C                   MOVE      Q#SECL        XHSECL
     C                   MOVE      Q#SBYL        XHSBYL
     C                   MOVE      CMSTAT        XHSTAT
     C*
     C* TRANSFER ALL DARTS ADDITIONAL REMARKS ASSOCIATED WITH THIS
     C* 'FROM' MAJOR REMARK
     C*
     C     ARKYFR        CHAIN     PAR01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C                   MOVEL     QQTOKY        ARKEY
     C                   Z-ADD     XHSEQ#        ARSEQ#
     C                   MOVEL     KTPNAM        ARPNAM
     C                   MOVEL     Q1PGM         ARPGM
     C                   MOVE      DSTIME        ARTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HHDT
     C                   Z-ADD     HHDT          ARDT
     C                   ADD       1             ACNT
     C                   WRITE     PAR01
     C*
     C     ARKYFR        READE     PAR01                                  50
     C                   ENDDO                                                  END *IN50 DOWEQ '0'
     C*
     C* TRANSFER ALL DARTS CUSTOM GROUP ID RECORDS ASSOCIATED WITH
     C* THIS 'FROM' MAJOR REMARK
     C*
     C     CMFL05        IFEQ      'O'
     C*
     C     DLKYFR        CHAIN     PDL01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C                   MOVEL     QQTOKY        DLKEY
     C                   Z-ADD     XHSEQ#        DLSEQ#
     C                   MOVEL     Q1PGM         DLPGM
     C                   MOVE      DSTIME        DLTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HHDT
     C                   Z-ADD     HHDT          DLDT
     C                   WRITE     PDL01
     C*
     C     DLKYFR        READE     PDL01                                  50
     C                   ENDDO                                                   END *IN50 DOWEQ '0'
     C*
     C                   ENDIF                                                   END CMFL05 EQ 'O'
     C*** BEGIN ADD *** 01/04/00 ***********************************************
     C*
     C* Call program to copy the Multiple-Object records, if any.
     C*
     C     CMQPTY        IFEQ      'Y'
     C*
     C                   MOVE      XXFRS#        QQFSEQ            5
     C                   MOVE      XHSEQ#        QQTSEQ            5
     C                   MOVE      'T'           QXMODE            1
     C*
     C                   CALL      'SC917R'
     C                   PARM                    QQFRKY
     C                   PARM                    QQFSEQ
     C                   PARM                    QQTOKY
     C                   PARM                    QQTSEQ
     C                   PARM                    QXMODE
     C*
     C                   ENDIF
     C*** END   ADD *** 01/04/00 ***********************************************
     C*** Begin Chg * 05/22/02 ****************************************
     C*
     C* TRANSFER THE SELECTED DARTS MAJOR REMARK FROM THE 'FROM' KEY
     C* TO THE NEW 'TO' KEY
     C*
     C**                   MOVELQQTOKY    CMKEY
     C**                   MOVELXHSEQ#    CMSEQ#
     C**                   MOVE QQTORC    CCRMCD
     C**                   MOVE QQTODI    CMDIFL
     C*** BEG ADD *** 05/17/01 ****************************************
     C**                   Z-ADDXHSECL    CMSECL
     C**                   MOVE XHSBYL    CMSBYL
     C*** END ADD *** 05/17/01 ****************************************
     C*
     C***        QQTODU    IFNE *BLANKS
     C*
     C***                  MOVE QQTODU    CMUSER
     C*
     C***                  ENDIF                           END QQTODU NE *BLANKS
     C*
     C***        QQTODD    IFNE *ZEROS
     C*
     C***                  MOVE QQTODD    SRDTI
     C***                  CALL 'SRCVTDAT'DTCYMD
     C***                  MOVELSRDTO     CMCDAT
     C*
     C***                  ENDIF                           END QQTODD NE *BLANKS
     C*
     C***                  MOVELKTPNAM    CMPNAM
     C***                  MOVELQ1PGM     CMPGM
     C                   MOVE      DSTIME        CMTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HHDT
     C                   Z-ADD     HHDT          CMDT
     C                   ADD       1             RCNT
     C******               WRITEPCM01
     C***** BEGIN ADD **** 07/01/99 ***********************************
     C******               FEOD SDCMLA
     C***** END   ADD **** 07/01/99 ***********************************
     C*** End   Chg * 05/22/02 ****************************************
     C*
     C**** ADD 11/19/97 *** START ************ 48WJF ********************
     C** CALL PROGRAM TO PROCESS THE SEND OF DIARY MESSAGE
     C     CMUSER        IFNE      Q1USER
     C     CMDIFL        ANDEQ     'Y'
     C*
     C***  Begin Del  ***  01/25/13  **************************************
     C**********         MOVE      XHCDAT        #MTDDT
     C**********         MOVE      XHSEQ#        #MSEQ#
     C**********         CALL      'SCDMSGR '                           60
     C**********         PARM      Q#KEY         #MKEY            40
     C**********         PARM                    #MSEQ#            5
     C**********         PARM      Q#USER        #MTUSR           10
     C**********         PARM                    #MTDDT            6
     C**********         PARM      Q#RMCD        #MTRMC            5
     C**********         PARM      KTPNAM        #MTPGM           10
     C**********         PARM      *BLANK        #MRTC             1
     C***  End   Del  ***  01/25/13  **************************************
     C*
     C                   ENDIF
     C*
     C**** ADD 11/19/97 *** END ************** 48WJF ********************
     C*
     C                   ENDIF                                                  END *IN60 EQ '0'
     C*
     C* THE SELECTED 'FROM' REMARK HAS NOW BEEN TRANSFERRED, WE CAN
     C* NOW DELETE THE OLD 'FROM' REMARK
     C*
     C     CMKYFR        CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C* DELETE ALL THE ADDITIONAL REMARKS ASSOCIATED WITH THE 'FROM'
     C* MAJOR REMARK THAT HAS PREVIOUSLY BEEN TRANSFERRED
     C*
     C     ARKYFR        CHAIN     PAR01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C                   DELETE    PAR01
     C*
     C     ARKYFR        READE     PAR01                                  50
     C                   ENDDO                                                  END *IN50 DOWEQ '0'
     C*
     C* DELETE ALL THE CUSTOM GROUP IDS ASSOCIATED WITH THE 'FROM'
     C* MAJOR REMARK THAT HAS PREVIOUSLY BEEN TRANSFERRED
     C*
     C     CMFL05        IFEQ      'O'
     C*
     C     DLKYFR        CHAIN     PDL01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C                   DELETE    PDL01
     C*
     C     DLKYFR        READE     PDL01                                  50
     C                   ENDDO                                                   END *IN50 DOWEQ '0'
     C*
     C                   ENDIF                                                   END CMFL05 EQ 'O'
     C*** BEGIN ADD *** 11/29/04 **************************************
     C*
     C* DELETE THE ADDITIONAL INFO
     C*
     C     ##DEIF        IFEQ      'Y'
     C     AIKYFR        CHAIN     PAI01                              50
     C     *IN50         DOWEQ     '0'
     C                   DELETE    PAI01
     C     AIKYFR        READE     PAI01                                  50
     C                   ENDDO                                                   END *IN50 DOWEQ '0'
     C                   ENDIF
     C*** END   ADD *** 11/29/04 **************************************
     C*
     C* NOW THAT THE SELECTED 'FROM' DARTS MAJOR REMARK HAS BEEN
     C* TRANSFERRED, WE CAN DELETE THE ORIGINAL 'FROM' DARTS MAJOR
     C* REMARK
     C*
     C                   DELETE    PCM01
     C*
     C                   ENDIF                                                  END *IN60 EQ '0'
     C*
     C* ELSE 'TO' MAJOR REMARK DOES NOT EXIST, SO ALL 'FROM' REMARKS
     C* ADDITIONAL REMARKS, AND CUSTOM GROUP ID CAN BE UPDATED WITH
     C* THE 'TO' KEY VALUES
     C*
     C                   ELSE                                                   ELSE *IN60 EQ '1'
     C*
     C******               Z-ADD*ZEROS    XHSEQ#
     C                   Z-ADD     1             XHSEQ#
     C                   Z-ADD     XHSEQ#        XXTOS#
     C     CMKYFR        CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C* MOVE DARTS FIELDS TO HOLD FIELDS FOR USE LATER IN THE
     C* SUBROUTINE THAT WILL WRITE ADDITIONAL REMARKS
     C*
     C                   MOVE      CMCSID        XHCSID
     C                   Z-ADD     CMCDAT        XHCDAT
     C                   MOVE      CMCODE        XHCODE
     C                   MOVE      CMPNAM        XHPNAM
     C                   Z-ADD     CMSECL        XHSECL
     C                   MOVE      CMSBYL        XHSBYL
     C                   MOVE      CMSTAT        XHSTAT
     C*
     C* TRANSFER ALL DARTS ADDITIONAL REMARKS ASSOCIATED WITH THIS
     C* 'FROM' MAJOR REMARK
     C*
     C     ARKYFR        CHAIN     PAR01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C                   MOVEL     QQTOKY        ARKEY
     C                   Z-ADD     XHSEQ#        ARSEQ#
     C                   MOVEL     KTPNAM        ARPNAM
     C                   MOVEL     Q1PGM         ARPGM
     C                   MOVE      DSTIME        ARTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HHDT
     C                   Z-ADD     HHDT          ARDT
     C                   ADD       1             ACNT
     C                   UPDATE    PAR01
     C*
     C     ARKYFR        READE     PAR01                                  50
     C                   ENDDO                                                  END *IN50 DOWEQ '0'
     C*
     C* TRANSFER ALL DARTS CUSTOM GROUP IDS ASSOCIATED WITH THIS
     C* 'FROM' MAJOR REMARK
     C*
     C     CMFL05        IFEQ      'O'
     C*
     C     DLKYFR        CHAIN     PDL01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C                   MOVEL     QQTOKY        DLKEY
     C                   Z-ADD     XHSEQ#        DLSEQ#
     C                   MOVEL     Q1PGM         DLPGM
     C                   MOVE      DSTIME        DLTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HHDT
     C                   Z-ADD     HHDT          DLDT
     C                   UPDATE    PDL01
     C*
     C     DLKYFR        READE     PDL01                                  50
     C                   ENDDO                                                   END *IN50 DOWEQ '0'
     C*
     C                   ENDIF                                                   END CMFL05 EQ 'O'
     C*** BEGIN ADD *** 01/04/00 ***********************************************
     C*
     C* Call program to copy the Multiple-Object records, if any.
     C*
     C     CMQPTY        IFEQ      'Y'
     C*
     C                   MOVE      CMSEQ#        QQFSEQ            5
     C                   MOVE      XHSEQ#        QQTSEQ            5
     C                   MOVE      'T'           QXMODE            1
     C*
     C                   CALL      'SC917R'
     C                   PARM                    QQFRKY
     C                   PARM                    QQFSEQ
     C                   PARM                    QQTOKY
     C                   PARM                    QQTSEQ
     C                   PARM                    QXMODE
     C*
     C                   ENDIF
     C*** END   ADD *** 01/04/00 ***********************************************
     C*
     C* TRANSFER THE SELECTED DARTS MAJOR REMARK FROM THE 'FROM' KEY
     C* TO THE NEW 'TO' KEY
     C*
     C                   MOVEL     QQTOKY        CMKEY
     C                   Z-ADD     XXTOS#        CMSEQ#
     C                   MOVE      QQTODI        CMDIFL
     C*** BEG ADD *** 05/17/01 ****************************************
     C                   Z-ADD     XHSECL        CMSECL
     C                   MOVE      XHSBYL        CMSBYL
     C*** END ADD *** 05/17/01 ****************************************
     C*
     C     QQTODU        IFNE      *BLANKS
     C*
     C                   MOVE      QQTODU        CMUSER
     C*
     C                   ENDIF                                                  END QQTODU NE *BLANK
     C*
     C     QQTODD        IFNE      *ZEROS
     C*
     C                   MOVE      QQTODD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMCDAT
     C*
     C                   ENDIF                                                  END QQTODD NE *BLANK
     C*
     C                   MOVEL     KTPNAM        CMPNAM
     C                   MOVEL     Q1PGM         CMPGM
     C                   MOVE      DSTIME        CMTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HHDT
     C                   Z-ADD     HHDT          CMDT
     C                   ADD       1             RCNT
     C                   UPDATE    PCM01
     C**** ADD 11/19/97 *** START ************ 48WJF ********************
     C** CALL PROGRAM TO PROCESS THE SEND OF DIARY MESSAGE
     C     CMUSER        IFNE      Q1USER
     C     CMDIFL        ANDEQ     'Y'
     C*
     C***  Begin Del  ***  01/25/13  **************************************
     C**********         MOVE      XHCDAT        #MTDDT
     C**********         MOVE      CMSEQ#        #MSEQ#
     C**********         CALL      'SCDMSGR '                           60
     C**********         PARM      CMKEY         #MKEY            40
     C**********         PARM                    #MSEQ#            5
     C**********         PARM      CMUSER        #MTUSR           10
     C**********         PARM                    #MTDDT            6
     C**********         PARM      CMRMCD        #MTRMC            5
     C**********         PARM      CMPNAM        #MTPGM           10
     C**********         PARM      *BLANK        #MRTC             1
     C***  End   Del  ***  01/25/13  **************************************
     C*
     C                   ENDIF
     C*
     C**** ADD 11/19/97 *** END ************** 48WJF ********************
     C*
     C*
     C                   ENDIF                                                  END *IN60 EQ '0'
     C*
     C                   ENDIF                                                  END *IN60 EQ '0'
     C*
     C* PRINT OUT THE 'FROM' AND 'TO' KEYS, AND THE ADDITIONAL REMARK
     C* TOTALS, FOR THIS TRANSFER
     C*
     C                   EXCEPT    RMK
     C                   MOVE      QQFRKY        DTFRKY
     C                   MOVE      QQFRS#        DTFRS#
     C                   MOVE      QQFRRC        DTFRRC
     C                   MOVE      QQFRDI        DTFRDI
     C                   MOVE      QQFRDU        DTFRDU
     C                   MOVE      QQFRDD        DTFRDD
     C                   MOVE      QQTOKY        DTTOKY
     C*** CHG 04/04/03 *** MOVE CMSEQ#    DTTOS#
     C                   MOVE      XHSEQ#        DTTOS#
     C                   MOVE      QQTORC        DTTORC
     C                   MOVE      QQTODI        DTTODI
     C                   MOVE      QQTODD        DTTODD
     C                   MOVE      QQTODU        DTTODU
     C                   WRITE     PDT01
     C*
     C*** Begin Add *** 05/30/02 ***********************************************
     C     CMIMGF        IFNE      ' '
     C     CMIMGF        ANDNE     'W'
     C                   MOVE      QQFRS#        @@FRS#
     C                   CALL      'IS901R'                             90
     C                   PARM      CMIDOC        @@IDOC           12
     C                   PARM      CMFOLD        @@FOLD           80
     C                   PARM      QQFRKY        @@FKY            40
     C                   PARM                    @@FRS#            5 0
     C                   PARM      QQTOKY        @@TKY            40
     C                   PARM      XHSEQ#        @@TOS#            5 0
     C                   PARM      QQTYPE        @@TYPE            1
     C                   ENDIF
     C*** End   Add *** 05/30/02 ***********************************************
     C*
     C                   ENDSR
     C/EJECT
     C*******************************************************************
     C* WRITE ADDITIONAL REMARK SUBROUTINE                   W R T R M K*
     C*******************************************************************
     C     WRTRMK        BEGSR
     C*
     C* READ THE DARTS ADDITIONAL REMARKS FILE TO GET THE NEXT
     C* AVAILABLE SEQ# AND STORE IN XAHSEQ
     C*
     C     ARKYTO        SETGT     PAR01
     C                   READP(N)  PAR01                                  60
     C     *IN60         IFEQ      '0'
     C     QQTOKY        ANDEQ     ARKEY
     C     XXTOS#        ANDEQ     ARSEQ#
     C*
     C                   Z-ADD     *ZEROS        XAHSEQ
     C     ARASQ#        ADD       1             XAHSEQ
     C*
     C                   ELSE                                                   ELSE *IN60 EQ '0'
     C*
     C                   Z-ADD     1             XAHSEQ
     C*
     C                   END                                                    END *IN60 EQ '0'
     C*
     C* CREATE THE DARTS ADDITIONAL REMARKS RECORDS, FOR THIS KEY, TO
     C* INDICATE THAT THIS REMARK WAS COPIED OR TRANSFERRED FROM
     C* ANOTHER REMARK
     C*
     C                   CLEAR                   PAR01
     C                   MOVE      QQTOKY        ARKEY
     C                   Z-ADD     XHSEQ#        ARSEQ#
     C*
     C* CREATE THE REMARK, BASED UPON WHETHER REMARK WAS COPIED OR
     C* TRANSFERRED
     C*
     C     QQTYPE        IFEQ      'T'
     C*
     C                   MOVEL     RMRK(1)       ARCM75
     C     ARCM75        CAT       QQFRKY:2      ARCM75
     C*** Begin Add 06/18/03 ******************************************
     C     ARCM75        CAT       'By':2        ARCM75
     C     ARCM75        CAT       Q1USER:2      ARCM75
     C*** End   Add 06/18/03 ******************************************
     C*
     C                   ELSE                                                   ELSE QQTYPE EQ 'T'
     C*
     C                   MOVEL     RMRK(2)       ARCM75
     C     ARCM75        CAT       QQFRKY:2      ARCM75
     C*** Begin Add 06/18/03 ******************************************
     C     ARCM75        CAT       'By':2        ARCM75
     C     ARCM75        CAT       Q1USER:2      ARCM75
     C*** End   Add 06/18/03 ******************************************
     C*
     C                   END                                                    END QQTYPE EQ 'T'
     C*
     C                   Z-ADD     XAHSEQ        ARASQ#
     C                   MOVE      XHCSID        ARCSID
     C                   Z-ADD     XHCDAT        ARCDAT
     C                   MOVE      XHCODE        ARCODE
     C                   MOVE      KTPNAM        ARPNAM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HHDT
     C                   Z-ADD     HHDT          ARTRDT
     C                   Z-ADD     DSTIME        ARTRTM
     C                   Z-ADD     XHSECL        ARSECL
     C                   MOVE      XHSBYL        ARSBYL
     C                   MOVE      XHSTAT        ARSTAT
     C                   MOVE      Q1USER        ARUSER
     C                   MOVE      Q1PGM         ARPGM
     C                   Z-ADD     DSTIME        ARTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HHDT
     C                   Z-ADD     HHDT          ARDT
     C                   MOVE      Q1USER        ARIUSR
     C                   MOVE      QQTORC        ARRMCD
     C                   Z-ADD     ARDT          ARIDT
     C                   WRITE     PAR01
     C*
     C* WRITE THE SECOND LINE OF THE ADDITIONAL REMARK
     C*
     C                   CLEAR                   PAR01
     C                   MOVE      QQTOKY        ARKEY
     C                   Z-ADD     XHSEQ#        ARSEQ#
     C                   MOVEL     RMRK1(1)      ARCM75
     C     ARCM75        CAT       QQFRRC:1      ARCM75
     C     ARCM75        CAT       RMRK1(2):1    ARCM75
     C     ARCM75        CAT       QQFRDI:1      ARCM75
     C*
     C     QQFRDI        IFEQ      'Y'
     C*
     C     ARCM75        CAT       RMRK1(3):1    ARCM75
     C     ARCM75        CAT       QQFRDU:1      ARCM75
     C*
     C                   ENDIF                                                  END QQFRDI EQ 'Y'
     C*
     C     ARCM75        CAT       RMRK1(4):1    ARCM75
     C     ARCM75        CAT       XMFRDD:1      ARCM75
     C     ARCM75        CAT       '/':0         ARCM75
     C     ARCM75        CAT       XDFRDD:0      ARCM75
     C     ARCM75        CAT       '/':0         ARCM75
     C     ARCM75        CAT       XCFRDD:0      ARCM75
     C     ARCM75        CAT       XYFRDD:0      ARCM75
     C*
     C     XAHSEQ        ADD       1             ARASQ#
     C                   MOVE      XHCSID        ARCSID
     C                   Z-ADD     XHCDAT        ARCDAT
     C                   MOVE      XHCODE        ARCODE
     C                   MOVE      KTPNAM        ARPNAM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HHDT
     C                   Z-ADD     HHDT          ARTRDT
     C                   Z-ADD     DSTIME        ARTRTM
     C                   Z-ADD     XHSECL        ARSECL
     C                   MOVE      XHSBYL        ARSBYL
     C                   MOVE      XHSTAT        ARSTAT
     C                   MOVE      Q1USER        ARUSER
     C                   MOVE      Q1PGM         ARPGM
     C                   Z-ADD     DSTIME        ARTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HHDT
     C                   Z-ADD     HHDT          ARDT
     C                   MOVE      Q1USER        ARIUSR
     C                   MOVE      QQTORC        ARRMCD
     C                   Z-ADD     ARDT          ARIDT
     C                   WRITE     PAR01
     C*
     C                   ENDSR
     C/EJECT
     O*****************************************************************
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS, INC.'
     O***                                50 '1997'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              87
     O                       DSDATE        Y    120
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          114 'PAGE:'
     O                       PAGE          Z    120
     O          E            RMK            1
     O                                           62 'REMARK'
     O                                           69 'DIARY'
     O                                           82 'DIARY'
     O                                           96 'DIARY'
     O                                          112 '# OF REMARKS'
     O                                          131 '# OF ADDT''L'
     O          E            RMK            1
     O                                           55 'SEQ#'
     O                                           61 'CODE'
     O                                           68 'TYPE'
     O                                           82 'USER'
     O                                           96 'DATE'
     O               98                         112 '   COPIED  '
     O              N98                         112 'TRANSFERRED'
     O                                          129 'REMARKS'
     O          E            RMK            1
     O                                            5 'FROM:'
     O                       QQFRKY              48
     O                       @@FRS#        Z     55
     O                       QQFRRC              62
     O                       QQFRDI              66
     O                       QQFRDU              87
     O                       @@FRDD              99 '  /  /    '
     O                       RCNT          Z    107
     O                       ACNT          Z    127
     O*
     O          E            RMK            2
     O                                            5 '  TO:'
     O                       QQTOKY              48
     O                       XHSEQ#        Z     55
     O                       QQTORC              62
     O                       QQTODI              66
     O                       QQTODU              87
     O                       @@TODD              99 '  /  /    '
     O*
     C/EJECT
**  RMRK - REMARK TEXT
Remark Transferred:  From Key:
Remark Copied:       From Key:
**  RMRK1 - REMARK TEXT
Remark Code:
Diary Type:
Diary User:
Remark Dt:
