     H DFTACTGRP(*NO)

     FCLAIM01D  CF   E             WORKSTN
     F                                     SFILE(CLAIMSFL : RRN)

     FCLAIMSP   UF A E           K DISK
     FCLAIMS2L  IF   E           K DISK    RENAME(CLAIMS : CLAIMS2)

     D RRN             S              4S 0
     D PROGRAM         S             10A
     D ACTION          S             10A
     D DONE            S              1A
     D CMDT            S              6S 0

     C                   DOW       *IN03 = *OFF
     C                   EXSR      LOADSF
     C                   WRITE     BOTTOM
     C                   EXFMT     CLAIMCTL
     C                   MOVEL     *BLANKS       ACTION
     C
     C                   READC     CLAIMSFL                               99
     C                   IF        *IN99 = *OFF
     C                   SELECT
     C                   WHEN      OPTION = '2'
     C                   MOVEL     'change'      ACTION
     C                   EXSR      LOADD
     C                   EXSR      SHOWD
     C                   IF        *IN03 = *OFF AND *IN12 = *OFF
     C                   EXSR      SAVED
     C                   ENDIF
     C                   WHEN      OPTION = '4'
     C                   MOVEL     'delete'      ACTION
     C                   EXSR      LOADD
     C                   EXSR      DODEL
     C                   WHEN      OPTION = '5'
     C                   MOVEL     'display'     ACTION
     C                   EXSR      LOADD
     C                   EXSR      SHOWD
     C                   ENDSL
     C                   ENDIF
     C
     C                   IF        *IN06 = *ON
     C                   MOVEL     'add'         ACTION
     C                   EXSR      NEWR
     C                   EXSR      SHOWD
     C                   IF        *IN03 = *OFF AND *IN12 = *OFF
     C                   EXSR      SAVED
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     C
     C                   SETON                                        LR
     C                   RETURN

     C*=======================================================================
     C* Load Claims Subfile
     C*=======================================================================
     C     LOADSF        BEGSR
     C                   Z-ADD     0             RRN
     C                   SETON                                        70
     C                   WRITE     CLAIMCTL
     C                   SETOFF                                       70
     C
     C                   IF        C1DESC = *BLANKS
     C     KEY1          SETLL     CLAIMS
     C                   READ      CLAIMS                                 99
     C                   ELSE
     C     KEY2          SETLL     CLAIMS2
     C                   READ      CLAIMS2                                99
     C                   ENDIF
     C
     C                   DOW       *IN99 = *OFF
     C                   ADD       1             RRN
     C                   MOVEL     *BLANKS       OPTION
     C                   Z-ADD     CMNUMBER      S1NUMBER
     C     *MDY          MOVE      CMDATE        CMMDY             6 0
     C                   WRITE     CLAIMSFL
     C
     C                   IF        C1DESC = *BLANKS
     C                   READ      CLAIMS                                 99
     C                   ELSE
     C                   READ      CLAIMS2                                99
     C                   ENDIF
     C                   ENDDO
     C                   ENDSR

     C*=======================================================================
     C* Load Claim Details
     C*=======================================================================
     C     LOADD         BEGSR
     C                   IF        ACTION = 'change'
     C     KEY3          CHAIN     CLAIMS                             99
     C                   SETOFF                                       80
     C                   ELSE
     C                   SETON                                        80
     C     KEY3          CHAIN     CLAIMS                             99
     C                   ENDIF
     C
     C                   IF        *IN99 = *OFF
     C                   Z-ADD     CMEQEST       CMTOTEST          7 2
     C                   ADD       CMOTHEST      CMTOTEST
     C                   ADD       CMPROPEST     CMTOTEST
     C                   Z-ADD     CMEQCOST      CMTOTCOST         7 2
     C                   ADD       CMOTHCOST     CMTOTCOST
     C                   ADD       CMPROPCOST    CMTOTCOST
     C                   ELSE
     C                   Z-ADD     0             CMTOTEST
     C                   Z-ADD     0             CMTOTCOST
     C                   ENDIF
     C
     C     *MDY          MOVE      CMDATE        CMMDY
     C                   ENDSR

     C*=======================================================================
     C* Show Claim Details
     C*=======================================================================
     C     SHOWD         BEGSR
     C                   MOVEL     ACTION        MESSAGE
     C                   MOVEL     'N'           DONE
     C
     C                   DOU       DONE = 'Y'
     C                   MOVEL     *BLANKS       PROGRAM
     C                   MOVEL     'Y'           DONE
     C                   EXFMT     DETAIL
     C
     C                   IF        *IN06 = *ON
     C                   MOVEL     'CLAIM22R'    PROGRAM
     C                   ENDIF
     C                   IF        *IN07 = *ON
     C                   MOVEL     'CLAIM23R'    PROGRAM
     C                   ENDIF
     C                   IF        *IN08 = *ON
     C                   MOVEL     'CLAIM24R'    PROGRAM
     C                   ENDIF
     C                   IF        *IN09 = *ON
     C                   MOVEL     'CLAIM25R'    PROGRAM
     C                   ENDIF
     C                   IF        *IN10 = *ON
     C                   MOVEL     'CLAIM26R'    PROGRAM
     C                   ENDIF
     C                   IF        *IN11 = *ON
     C                   MOVEL     'CLAIM27R'    PROGRAM
     C                   ENDIF
     C                   IF        *IN14 = *ON
     C                   MOVEL     'CLAIM28R'    PROGRAM
     C                   ENDIF
     C                   IF        *IN15 = *ON
     C                   MOVEL     'CLAIM29R'    PROGRAM
     C                   ENDIF
     C
     C                   IF        PROGRAM <> *BLANKS
     C                   MOVEL     'N'           DONE
     C                   CALL      PROGRAM                                98
     C                   PARM                    CMNUMBER
     C                   ENDIF
     C                   ENDDO
     C                   ENDSR

     C*=======================================================================
     C* Save Changes
     C*=======================================================================
     C     SAVED         BEGSR
     C                   Z-ADD     CMMDY         CMDT
     C     *MDY          MOVE      CMDT          CMDATE
     C
     C                   IF        ACTION = 'add'
     C                   WRITE     CLAIMS
     C                   ENDIF
     C
     C                   IF        ACTION = 'change'
     C                   UPDATE    CLAIMS
     C                   ENDIF
     C                   ENDSR

     C*=======================================================================
     C* Process Delete Option
     C*=======================================================================
     C     DODEL         BEGSR
     C                   EXFMT     CONFIRMDEL
     C                   IF        *IN12 = *OFF
     C     KEY3          CHAIN     CLAIMS                             99
     C                   IF        *IN99 = *OFF
     C                   DELETE    CLAIMS
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR

     C*=======================================================================
     C* Create New Record
     C*=======================================================================
     C     NEWR          BEGSR
     C                   Z-ADD     0             CMNUMBER
     C     *HIVAL        SETGT     CLAIMS
     C                   READP     CLAIMS                                 99
     C                   Z-ADD     CMNUMBER      S1NUMBER
     C                   Z-ADD     S1NUMBER      CMNUMBER
     C                   ADD       1             CMNUMBER
     C
     C                   SETOFF                                       80
     C                   TIME                    CMDATE
     C     *MDY          MOVE      CMDATE        CMMDY
     C                   TIME                    CMTIME
     C                   MOVEL     *BLANKS       CMDESC
     C                   Z-ADD     0             CMUNITS
     C                   Z-ADD     0             CMINJURED
     C                   MOVEL     'N'           CMHITRUN
     C                   MOVEL     'N'           CMCOMPLETE
     C                   MOVEL     *BLANKS       CMACTYPE
     C                   MOVEL     *BLANKS       CMCOUNTY
     C                   MOVEL     *BLANKS       CMADDR2
     C                   MOVEL     *BLANKS       CMADDR1
     C                   MOVEL     *BLANKS       CMCITY
     C                   MOVEL     *BLANKS       CMSTATE
     C                   Z-ADD     0             CMZIP
     C                   Z-ADD     0             CMEQEST
     C                   Z-ADD     0             CMOTHEST
     C                   Z-ADD     0             CMPROPEST
     C                   Z-ADD     0             CMEQCOST
     C                   Z-ADD     0             CMOTHCOST
     C                   Z-ADD     0             CMPROPCOST
     C                   MOVEL     'N'           CMMOTOR
     C                   MOVEL     'N'           CMFLATTIRE
     C                   MOVEL     'N'           CMBROKENW
     C                   MOVEL     'N'           CMVANDAL
     C                   MOVEL     'N'           CMPEDESTR
     C                   ENDSR

     C*=======================================================================
     C* Init
     C*=======================================================================
     C     *INZSR        BEGSR
     C     KEY1          KLIST
     C                   KFLD                    C1NUMBER
     C     KEY2          KLIST
     C                   KFLD                    C1DESC
     C     KEY3          KLIST
     C                   KFLD                    S1NUMBER
     C                   ENDSR
