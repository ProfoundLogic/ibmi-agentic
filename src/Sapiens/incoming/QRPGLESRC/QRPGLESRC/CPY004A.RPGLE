      *  (C) 2012 StoneRiver Inc. All rights reserved.
      *   StoneRiver is a trademark of StoneRiver, Inc.
      *
      *****************************************************************
     H/TITLE CPY004A - Write test env files - Create emp detail
      *****************************************************************
      *
      * DESCRITPION:  This pgm copies affected data for testing purposes.
      *
      * CREATED:      06/14/12  C:180    SUPPORT 79581
      ********************************************************************
      * REVISIONS:
      *
     FXWMCMP    IF   E             DISK
     FWDELL     IF   E           K DISK
     FXWDELP    O    E           K DISK    RENAME(PEL01:XPEL01)
     D XXCO#           S              3  0 INZ(777)
     D Z               S              5  0 INZ
     D @EMP            S             23    DIM(2000)
     D EMPNBR          DS
     D   XACO#                        3
     D   XAFND                        3
     D   XAEMP#                       9
     D   XADIV                        5
     D   XAFYR                        3


     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR

     C                   READ      XWMCMP
     C                   DOW       NOT %EOF(XWMCMP)
     C                   EXSR      WRTEMP
     C                   READ      XWMCMP
     C                   ENDDO
     C                   EVAL      *INLR = *ON
     C**********************************************************************
     C*             Write Employer detail
     C**********************************************************************
     C     WRTEMP        BEGSR
     C                   MOVE      CMCO#         XACO#
     C                   MOVE      CMFND         XAFND
     C                   MOVE      CMEMP#        XAEMP#
     C                   MOVE      CMDIV         XADIV
     C                   MOVE      CMFYR         XAFYR
     C     EMPNBR        LOOKUP    @EMP                                   50
     C                   IF        NOT %EQUAL
     C                   ADD       1             Z
     C                   MOVE      EMPNBR        @EMP(Z)
     C                   CLEAR                   XPEL01
     C     ELKY          SETLL     WDELL
     C     ELKY          READE     WDELL
     C                   DOW       NOT %EOF(WDELL)
     C                   WRITE     XPEL01
     C     ELKY          READE     WDELL
     C                   ENDDO
     C                   ENDIF
     C                   ENDSR
