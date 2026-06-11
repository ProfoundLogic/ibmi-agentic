     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H*****************************************************************
     H/TITLE W0051R64 - Activity status code/ & processing.
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0051R - WORK WITH POLICY YEARS                            *
     H*                                                               *
     H*  CREATED: 06/16/11  PROGRAMMER: C:235 PROJECT: 133WLI         *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     FW0051F64  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     Fwdelp     UF   E             DISK
     Fwtavl     IF   E           K DISK
     Fwmexl     IF   E           K DISK
     F                                     RENAME(PEX01:PEX02)
     Fwtavp     IF   E             DISK
     F                                     RENAME(PAV01:PAV02)
     Fwtbsp     IF   E           K DISK
     F                                     RENAME(PBS01:PBS02)
      *
     D psds          esds                  extname(szq1p)
     *
     D                 ds
     D  dsdttm                 1     14  0
     D  dstime                 1      6  0
     D  dsdate                 7     14  0
     D*
     D                 DS
     D  qckey                  1     40
     D  qcco#                  1      3  0
     D  qcemp#                 7     15  0
     D  qcdiv                 16     20  0
     D  qcqte                 40     40
     D  qcfnd                  4      6  0
      *
     D optnm           c                   'Change Actvity Status'
     D ASCTXT          C                   CONST('ACTIVITY STATUS CODE CHANGED')
     D ASCTX1          C                   CONST('Activity Status Changed from')
     D ASCTX2          C                   CONST(' by user ')
      *
     DWriteDarts       PR
     D  dkey                         40
     D  comm                        256
     D  rmcd                         10
      *
     C     *LIKE         DEFINE    ELASCD        XXASCD
     C     *LIKE         DEFINE    BSKEY         XXKEY
     C     *LIKE         DEFINE    EXASRM        SVASRM
      *
     C*===============================================================
     C**PLIST Definitions
     C*===============================================================
      *
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          wdelp
     C                   PARM                    @@RRN2            9 0          wdelp
      *
     C     ACDPL         PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKl         @@Kl              1
     C                   PARM      '2'           CALL              1
     C                   PARM                    @@RRN             9 0
      *
     C     dtcymd        plist
     C                   parm                    srdti             8 0
     C                   parm      *BLANKS       srffmt            8
     C                   parm      '*CYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
      *
     C*===============================================================
     C**KLIST Definitions
     C*===============================================================
     C     TBKY          KLIST
     C                   KFLD                    ElCO#
     C                   KFLD                    XXKEY
      *
     C     FNKY          KLIST
     C                   KFLD                    ElCO#
     C                   KFLD                    ELFND
     C*===============================================================
     C**MAIN
     C*===============================================================
     C*
     C                   exsr      pgmstr
     c                   select
     c                   when      @@mode = 'A' OR @@mode = 'C'
     C                   MOVE      '1'           *in27
     C                   exsr      mvdbsc
     C     start         tag
     C                   exfmt     wdw01
     C*
     C                   if        *INKJ= '1'
     C                   exsr      othfks
     C                   endif
     C*
     C                   if        *INKL<>'1'
     C                   exsr      errck1
     C*
     C                   if        *in90 = *off
     C*
     C* DARTS
     C                   if        scascd <> elascd
     C                             and *in90 = *off
     C                   exsr      darts
     C                   endif
     C*
     C                   exsr      updatr
     C                   endif
     C                   endif                                                  *INKL<>'1'
     C*
     c                   when      @@mode = 'D'
     C                   MOVE      '1'           *in26
     C                   MOVE      '0'           *in27
     C                   exsr      mvdbsc
     c                   dow       *inkl='0'
     C                   exfmt     wdw01
     c                   enddo
     c                   endsl
     C*
     C                   seton                                            lr
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
     C*===============================================================
     C     pgmstr        begsr
     c*
     C     @@rrn2        chain     pel01                              90
     C                   endsr
     c
     C*===============================================================
     C* ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
     C*===============================================================
     C*
     C     errck1        begsr
     C*
     C     SCASCD        IFNE      *BLANKS
     C                   movel(p)  SCASCD        xxkey
     C     tbky          chain     pav01                              90
     c                   if        *in90='1'
     c                   seton                                        25  90
     C                   goto      start
     c                   else
     C                   movel(p)  AVDESC        SCACTD
     c                   endif
     c                   endif
     C*
     C     SCASCD        ifeq      *blanks
     C                   movel(p)  *blanks       SCACTD
     c                   endif
     C*
     C                   endsr
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=SEARCH
     C*
     C     othfks        begsr
     C*
     C                   select
     C     HFLD          WHENEQ    'SCASCD'
     C                   clear                   @@rrn
     C                   call      'W2164R'      ACDPL                    99
     C     @@rrn         chain     wtavp                              90
     C                   movel(p)  avkey         scascd
     C                   movel(p)  avdesc        scactd
     C                   goto      start
     c                   endsl
     C*
     C                   endsr
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
     C     mvdbsc        begsr
     C*
     C                   movel     ELASCD        scascd
     C                   clear                   avdesc
     C                   movel     ELASCD        xxkey
     C     tbky          chain     pav01                              90
     C                   movel(p)  avdesc        scactd
     C*
     C                   endsr
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     mvscdb        begsr
     C                   movel     scascd        ELASCD
     C                   endsr
     C*===============================================================
     C* UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
     C*===============================================================
     C*
     C     updatr        begsr
     C*
     C                   exsr      mvscdb
     C*
     C                   movel     q1user        eluser
     C                   movel     q1pgm         elpgm
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         eldt
     C                   movel     dstime        eltime
     C*
     C                   update    PEL01
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* DARTS  - SUBROUTINE FOR WRITING DATA BASE RECORDS
     C*===============================================================
     C*
     C     darts         begsr
     C*
     C     FNKY          chain     pex02                              90
     C  N90              move      EXASRM        SVASRM
     C*
     C                   move      *blanks       xxLin2           80
     C                   move      *blanks       xxComm          256
     C                   move      *blanks       xxRmcd           10
     C*
     C* Load Line 1 text
     C                   movel     ASCTXT        xxLin1           35
     C* Build and load Line 2 text
     C                   if        ELASCD = *BLANKS
     C                   movel     'blank'       xxFrCd            5
     C                   else
     C                   movel(P)  ELASCD        xxFrCd
     C                   endif                                                  ELASCD = *BLANKS
     C*
     C                   if        SCASCD = *BLANKS
     C                   movel     'blank'       xxToCd            5
     C                   else
     C                   movel(P)  SCASCD        xxToCd
     C                   endif                                                  SCASCD = *BLANKS
     C*
     C                   eval      xxLin2 = ASCTX1 + ' ' + %TRIM(xxFrCd)
     C                                    + ' to ' + %TRIM(xxToCd)
     C                                    + ASCTX2 + %TRIM(q1user)
     C*
     C                   eval      xxComm = xxLin1 + xxLin2
     C*
     C                   move      *BLANKS       QCKEY
     C                   move      ELCO#         qcco#
     C                   move      ELFND         qcfnd
     C                   move      ELEMP#        qcemp#
     C                   move      ELDIV         qcdiv
     C*
     C                   eval      xxRmcd = SVASRM
     C*
     C* Call procedure to write DARTS
     C                   CALLP     WriteDarts(qckey:xxComm:xxRmcd)
     C*
     C                   endsr
     C*====================================================================
    C* Write DARTS remark
     C*====================================================================
     PwriteDarts       b

     DwriteDarts       pi
     D inDkey                        40
     D inComm                       256
     D inRmcd                        10

     D*
     D q#apid          s              2
     D q#cdat          s              8
     D q#code          s              2
     D q#comm          s            256
     D q#com2          s             19
     D q#csid          s              2
     D q#dcid          s             12
     D q#dctp          s              5
     D q#difl          s              1
     D q#fl01          s              1
     D q#fl02          s              1
     D q#fold          s             80
     D q#idoc          s             12
     D q#imgf          s              1
     D q#ipgm          s             10
     D q#ipth          s             10
     D q#ique          s             10
     D q#key           s             40
     D q#pgm           s             10
     D q#pidx          s              5
     D q#pnam          s             10
     D q#qpty          s              1
     D q#rmcd          s              5
     D q#sbyl          s              1
     D q#secl          s              3
     D q#seq#          s              5
     D q#user          s             10
    C*
     C                   eval      q#Key  = inDKey
     C                   eval      q#Comm = inComm
     C                   eval      q#RmCd = inRmcd
     C*
     C                   TIME                    DSDTTM
     C                   movel     dsdate        q#cdat
     C*
     C                   eval      q#PNam = 'W0051R64'
     C                   eval      q#User = q1User
     C                   eval      q#Pgm  = q1pgm
     C                   eval      q#IPgm = q1pgm
     C                   eval      q#Seq# = '00000'
     C                   eval      q#Sbyl = 'N'
     C                   eval      q#ImgF = ' '
     C*
     C                   eval      q#Fold = *blanks
     C                   eval      q#DcId = *blanks
     C                   eval      q#DiFl = ' '
     C*
     C                   clear                   q#com2
     C                   clear                   q#secl
     C                   clear                   q#fl01
     C                   clear                   q#fl02
     C                   clear                   q#apid
     C                   clear                   q#idoc
     C                   clear                   q#ique
     C                   clear                   q#ipth
     C                   clear                   q#pidx
     C                   clear                   q#qpty
     C                   clear                   q#dctp
     C                   clear                   q#csid
     C                   clear                   q#code
     *
     C                   call      'DA700R'
     C                   parm                    q#key
     C                   parm                    q#comm
     C                   parm                    q#cdat
     C                   parm                    q#dcid
     C                   parm                    q#pnam
     C                   parm                    q#secl
     C                   parm                    q#user
     C                   parm                    q#pgm
     C                   parm                    q#fold
     C                   parm                    q#difl
     C                   parm                    q#ipgm
     C                   parm                    q#fl01
     C                   parm                    q#fl02
     C                   parm                    q#rmcd
     C                   parm                    q#seq#
     C                   parm                    q#sbyl
     C                   parm                    q#com2
     C                   parm                    q#apid
     C                   parm                    q#idoc
     C                   parm                    q#ique
     C                   parm                    q#imgf
     C                   parm                    q#ipth
     C                   parm                    q#pidx
     C                   parm                    q#qpty
     C                   parm                    q#dctp
     C                   parm                    q#csid
     C                   parm                    q#code

     PwriteDarts       e
