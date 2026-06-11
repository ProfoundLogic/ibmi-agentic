     H*  (c) Copyright Stone River, Inc., 2009
     H*****************************************************************
     H/TITLE CPYCLTDTA - Copy from client table file to generic table file
     H option(*nodebugio:*srcstmt)
     H*****************************************************************
     H* Created on 09/29/2009 by c:62 for Support WC135 67535
     H*****************************************************************
     H* Research:
     H*
     H*****************************************************************
     FWTCIP     IF   E             DISK
     FWTVLL     IF   E           k DISK
     FWTGIL     IF a E           k DISK
     FWTGTL     IF a E           k DISK
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     C*
     C     GIKY          KLIST
     C                   KFLD                    CICO#
     C                   KFLD                    CITBID
      *
     C     GTKY          KLIST
     C                   KFLD                    VLCO#
     C                   KFLD                    VLTBID
     C                   KFLD                    VLKEY
      *
     C     VLKY          KLIST
     C                   KFLD                    CICO#
     C                   KFLD                    CITBID
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*================================================================
     C*      M  A I  N    L  O  G  I  C
     C*================================================================
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C*
     C                   EXSR      WRITGI
     C*
     C                   MOVE      *ON           *INLR
     C***************************************************************
     C* WRITE FROM WTCIP TO WTGIP
     C***************************************************************
     C     WRITGI        BEGSR
     C*
     C                   read      pci01
     C                   if        not %eof
     C                   dou       %eof
     C*
     C     giky          chain     pgi01
     C                   if        not %found
     C*
     C                   eval      gico#=cico#
     C                   eval      gitbid=citbid
     C                   eval      gidesc=cidesc
     C                   eval      gifn01=cifn01
     C                   eval      gistat=cistat
     C                   eval      giuser=ciuser
     C                   eval      gipgm=cipgm
     C                   eval      gidt=cidt
     C                   eval      gitime=citime
     C                   write     pgi01
     C*
     C                   exsr      writgt
     C*
     C                   endif
     C*
     C                   read      pci01
     C                   enddo
     C                   endif
     C*
     C                   ENDSR
     C***************************************************************
     C* WRITE FROM WTVCP TO WTGTP
     C***************************************************************
     C     WRITGT        BEGSR
     C*
     C     vlky          chain     pvl01
     C                   if        %found
     C                   dou       %eof
     C*
     C     gtky          chain     pgt01
     C                   if        not %found
     C*
     C                   eval      gtco#=vlco#
     C                   eval      gttbid=vltbid
     C                   eval      gtkey=vlkey
     C                   eval      gtdesc=vldesc
     C*
     C                   eval      gtfg01=vlfg01
     C                   eval      gtfg02=vlfg02
     C                   eval      gtfg03=vlfg03
     C                   eval      gtfg04=vlfg04
     C                   eval      gtfg05=vlfg05
     C                   eval      gtfg06=vlfg06
     C                   eval      gtfg07=vlfg07
     C                   eval      gtfg08=vlfg08
     C                   eval      gtfg09=vlfg09
     C                   eval      gtfg10=vlfg10
     C*
     C                   eval      gtcf01=vlcf01
     C                   eval      gtcf02=vlcf02
     C                   eval      gtcf03=vlcf03
     C                   eval      gtcf04=vlcf04
     C                   eval      gtfg05=vlfg05
     C*
     C                   eval      gtn501=vln501
     C                   eval      gtn502=vln502
     C                   eval      gtn503=vln503
     C                   eval      gtn504=vln504
     C                   eval      gtn505=vln505
     C*
     C                   eval      gtn901=vln901
     C                   eval      gtn902=vln902
     C                   eval      gtn903=vln903
     C                   eval      gtn904=vln904
     C                   eval      gtn905=vln905
     C*
     C                   eval      gtnb01=vlnb01
     C                   eval      gtnb02=vlnb02
     C                   eval      gtnb03=vlnb03
     C                   eval      gtnb04=vlnb04
     C*
     C                   eval      gtstat=vlstat
     C                   eval      gtuser=vluser
     C                   eval      gtpgm=vlpgm
     C                   eval      gtdt=vldt
     C                   eval      gttime=vltime
     C*
     C                   write     pgt01
     C*
     C                   endif
     C*
     C     vlky          reade     pvl01
     C                   enddo
     C                   endif
     C*
     C                   ENDSR
