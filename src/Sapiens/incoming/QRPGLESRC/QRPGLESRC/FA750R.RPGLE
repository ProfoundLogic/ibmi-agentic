     H* Copyright (C) Fiserv, Inc., 2007
    H*********************************************************************
    H/TITLE P2P Acknowledgement Generation Process
     H*OPTION(*NODEBUGIO: *SRCSTMT)
     H*DEBUG(*YES)
    H*********************************************************************
    H*  CALLED FROM: FA750C                                              *
    H*                                                                   *
    H*  CREATED: 08/01/07      C:193     PROJECT: MC299                  *
    H*********************************************************************
    H*  REVISIONS:                                                       *
    H*                                                                   *
    H*********************************************************************
     FFAU1L9    IF   E           K DISK
     FFAUHL     IF   E           K DISK
     FFAAKP1    O    E             DISK
     FFAAKP2    O    E             DISK
     D akdata         uds
     D xxdate                        10a
     F*
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)

     D DTTMDS          DS
     D   HMS                          6
     D   MMDD                         4
     D   YEAR                         4

     D*
     D HIT             S               n
     D CLNM            S             10A   DTAARA(CLNAME)
     D TEMP            S             14A
     D DTTM            S             14  0
     D DTTMC           S             14A
     D @@name          S                   like(u1name)
     D kkname          S                   like(u1name)
     D kkprdt          S                   like(u1prdt)
     D date            s               d   datfmt(*usa)
     D date_mdyy       s               d   inz datfmt(*usa)
     D*
     C***************** KEYLISTS ******************************************
     C*** KEYLIST FOR FAUHL - BATCH PROCESSING FILE
     C*
     C     UHKY          KLIST
     C                   KFLD                    KKNAME
     C*
     C*** KEYLIST FOR FAU1L - PROVIMA PAYMENT FILE
     C*
     C     U1KY          KLIST
     C                   KFLD                    KKPRDT
     C*
     C*********************************************************************
     C*** PARAMETERS RECIEVED
     C     *ENTRY        PLIST
     C                   PARM                    @@NAME
     C                   PARM                    @@AK1D          100
     C                   PARM                    @@AK2D          100
     C                   PARM                    @@CALL            1
     C*
     C***********   MAIN PROCESSING STARTS   ******************************
     C*
     C                   IN        CLNM
     C*
     C                   IF        @@CALL = '1'
     C                   EXSR      SRWRTAK1
     C                   ENDIF
     C*
     C                   IF        @@CALL = '2'
     C                   EXSR      SRWRTAK2
     C                   ENDIF
     C*
     C                   EVAL      *INLR = *ON
     C***********   MAIN PROCESSING ENDS     ******************************
     C*********************************************************************
     C*** SUBROUTINE TO WRITE ACKNOWLEDGEMENT 1 FILE - FAAKP1
     C*********************************************************************
     C     SRWRTAK1      BEGSR
     C*
     C                   EVAL      KKNAME=@@NAME
     C     UHKY          CHAIN     PUH01                              60
     C*
     C                   EVAL      AKBAT# = UHBAT#
     C                   TIME                    DTTM
     C                   MOVE      DTTM          DTTMDS
     C                   EVAL      DTTMC= YEAR + MMDD + HMS
     C                   MOVEL     DTTMC         AKDTTM
     c                   EVAL      HMS='110101'
     C                   EVAL      DTTMC= YEAR + MMDD + HMS
     C*                  TIME                    AKDTTM
     C                   EVAL      AKORIG = 'WCMS'
     C                   EVAL      AKSTAT = '0'
     C*
     C                   WRITE     PAK01
     C*
     C*                  MOVE      AKDTTM        DTTMC
     C***                EVAL      TEMP = 'P2P_' + CLNM
     C                   EVAL      TEMP = %trim(CLNM) + '_P2P'
     C                   EVAL      @@AK1D = %trim(TEMP)+'_'+DTTMC+'.ak1'
     C*
     C                   ENDSR
     C*********************************************************************
     C*********************************************************************
     C*** SUBROUTINE TO WRITE ACKNOWLEDGEMENT 2 FILE - FAAKP2
     C*********************************************************************
     C     SRWRTAK2      BEGSR
     C*
     C                   eval      hit=*off
     C*
     C                   time                    dsdttm
     C                   move      xxdate        date_mdyy
     C     *cymd         move      date_mdyy     kkprdt
     C     u1ky          setgt     PU101
     C                   read      PU101
     C                   dow       not %eof
     c                   if        u1ntwk='P2P' or u1ntwk='DCRX'
     C*
     C                   eval      hit=*on
     C                   EVAL      KKNAME=U1NAME
     C     UHKY          CHAIN     PUH01                              60
     C*
     C                   EVAL      AKBAT# = UHBAT#
     C                   TIME                    DTTM
     C                   MOVE      DTTM          DTTMDS
     C                   EVAL      DTTMC= YEAR + MMDD + HMS
     C                   MOVEL     DTTMC         AKDTTM
     c                   EVAL      HMS='110101'
     C                   EVAL      DTTMC= YEAR + MMDD + HMS
     C*                  TIME                    AKDTTM
     C                   EVAL      AKORIG = CLNM
     C*
     C                   IF        U1PRFL='Y'
     C                   EVAL      AKSTAT = 'A'
     c                   move      *blanks       akreas
     C                   ELSE
     C                   EVAL      AKSTAT = 'R'
     c                   eval      akreas = 'No Claim Found'
     C                   ENDIF
     C*
     C                   EVAL      AKTRID = U1TRID
     C*
     C                   WRITE     PAK02
     C*
     c                   endif
     C                   read      PU101
     C                   enddo
     C*
     C                   if        hit
     C                   EVAL      TEMP = %trim(CLNM) + '_' + 'P2P'
     C                   EVAL      @@AK2D = %trim(TEMP)+'_'+DTTMC+'.ak2'
     C                   endif
     C*
     C                   move      dsdate        date_mdyy
     C*
     C                   if        dstime < 120000
     C                   subdur    1:*days       date_mdyy
     C                   endif
     C*
     C                   move      date_mdyy     xxdate
     C*
     C                   ENDSR
     C**********************************************************************
