     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/TITLE table file setup                                                              *
     H**************************************************************************************
     H*  Description:  This program will create the Table file setup for WC1880            *
     H*                                                                                    *
     H*  CREATED: 09/09/10  PROGRAMMER: C:189 PROJECT: SY190/WC1880                        *
     H*                                                                                    *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H**************************************************************************************
     FWDF2P     IF   E           K DISK
     FWTGIL     IF A E           K DISK
     FWTGTL     IF A E           K DISK
     FWMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     D
     D*  Date/Time Data Structure.
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
    D*
     D OVRAR           S             10    DIM(99)
     D psds          esds                  extname(szq1p)
    C
     C*  KEY LISTS
     C*
     C     GIKY          KLIST
     C                   KFLD                    COCO#
     C                   KFLD                    XXTBID
     C*
     C     GTKY          KLIST
     C                   KFLD                    COCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    OVRAR(I)
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
    C*MAINLINE STARTS HERE
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C                   time                    dsdttm
     C*
     C                   EXSR      VLDGIP
     C                   EXSR      VLDGTP
     C                   EXSR      WRTGTP
    C
    C                   seton                                        lr
    C                   return
     C*====================================================================
     C*    CHECK IF WTGIP record exists for COVL, If not write it
     C*====================================================================
     C     VLDGIP        BEGSR
    C
    C                   MOVEL     'COVL'        XXTBID            4
    C
     C     *LOVAL        SETLL     PCO01
     C                   READ      PCO01                                  60
     C                   DOW       *IN60 = *OFF
    C
     C* Check the System Table file
    C     GIKY          CHAIN     PGI01                              60
     C                   IF        *IN60 = *ON
    C
     C                   MOVE      COCO#         GICO#
     C*
     C                   MOVEL     XXTBID        GITBID
    C                   EVAL      GIDESC = ' CERTIFICATE OF INSURANCE OVERLAY'
    C                   EVAL      GIFN01 = ' OVERLAY:'
    C
     C                   MOVE      'A'           GISTAT
     C                   MOVE      Q1USER        GIUSER
     C                   MOVE      Q1PGM         GIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GIDT
     C                   Z-ADD     DSTIME        GITIME
    C
    C                   WRITE     PGI01
     C                   ENDIF
    C
     C                   READ      PCO01                                  60
     C                   ENDDO
    C
     C                   ENDSR
     C*====================================================================
     C*    CHECK IF WTGTP record exists ,if not write it
     C*====================================================================
     C     VLDGTP        BEGSR
    C
     C*----------------------------------------------------------------------
    C* Create an array for the "Certificate of Insurance Overlay" values which
    C* are already existing one the client machine in addtion to 'CERT01' and 'CERTNC'
     C*----------------------------------------------------------------------
    C*                  EVAL      I = 0
    C*
    C*First add the two overlays to the array
    C*
    C                   ADD       1             I
     C                   MOVEL     'CERT01'      OVRAR(I)
    C
    C                   ADD       1             I
     C                   MOVEL     'CERTNC'      OVRAR(I)
    C
     C*  Read through fund file to check if any other Overlay is also being used by client.
    C*
     C                   READ      PF201
     C                   DOW       NOT %EOF(WDF2P)
    C
     C                   IF        F2CEOV <> ' ' AND  F2CEOV <> 'CERT01' AND
     C                             F2CEOV <> 'CERTNC'
    C*
    C                   MOVEL     F2CEOV        XXCEOV           10
     C     XXCEOV        LOOKUP    OVRAR                                  50
    C*
     C                   IF        *IN50 = *OFF
    C                   ADD       1             I                 2 0
     C                   MOVEL     F2CEOV        OVRAR(I)
     C                   MOVEL     F2CEOV        OVRAR(I)
     C                   ENDIF
     C                   ENDIF
    C
     C                   READ      PF201                                        WDF2P
     C                   ENDDO
    C
     C                   ENDSR
     C*====================================================================
     C*    READ ARRAY AND WRITE WTGTP RECORDS FOR VALUES, NOT EXISTING IN FILE
     C*====================================================================
     C     WRTGTP        BEGSR
     C
     C     *LOVAL        SETLL     PCO01
     C                   READ      PCO01                                  60
     C                   DOW       *IN60 = *OFF
     C
    C                   EVAL      I= 1
     C
     C                   Dou       OVRAR(I) =  ' '
     C
    C     GTKY          Chain     PGT01                              60
     C                   IF        *in60 = *on
     C
     C                   SELECT
     C     OVRAR(I)      WHENEQ    'CERT01'
     C                   EVAL      GTDESC = 'Base Overlay'
     C     OVRAR(I)      WHENEQ    'CERTNC'
     C                   EVAL      GTDESC = 'NC Overlay'
     C                   OTHER
     C                   EVAL      GTDESC = *BLANKS
     C                   ENDSL
     C
     C                   MOVE      COCO#         GTCO#
     C                   MOVE      XXTBID        GTTBID
     C                   MOVE      OVRAR(I)      GTKEY
     C                   MOVE      'A'           GTSTAT
     C
     C                   MOVE      Q1USER        GTUSER
     C                   MOVE      Q1PGM         GTPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GTDT
     C                   Z-ADD     DSTIME        GTTIME
     C                   WRITE     PGT01
     C                   ENDIF
     C
    C                   EVAL      I= I+1
     C                   ENDDO
     C
     C                   READ      PCO01                                  60    SMCOP LOOP
     C                   ENDDO
     C
     C                   ENDSR
