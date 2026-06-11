      *  (C) 2012 StoneRiver Inc. All rights reserved.
      *   StoneRiver is a trademark of StoneRiver, Inc.
      *
      *****************************************************************
     H/TITLE FIXD005H - Omit cases with multiple resv codes
      *****************************************************************
      *
      * DESCRIPTION:  This omits cases with multiple reserve codes
      *
      * CREATED:      06/14/12  C:180    SUPPORT 79581
      ********************************************************************
      * REVISIONS:
      *
     FBADPV5    IP   E           K DISK
     FBADPV2L   UF   E           K DISK    RENAME(BADPV2:BADPVR) PREFIX(x)
     FBADPV2RL  UF   E           K DISK    RENAME(BADPV2R:BADPVRR) PREFIX(x)
     D @RECD           S              2    DIM(10) INZ ASCEND
     D J               S              3  0 INZ
     IBADPV5R       01
     I                                          P5FND         L1
     I                                          P5FYR         L1
     I                                          P5CASE        L1

     C     PV2KY         KLIST
     C                   KFLD                    P5FND
     C                   KFLD                    P5FYR
     C                   KFLD                    P5CASE

     C   L1              MOVE      *ZEROS        J
     C   L1              CLEAR                   @RECD

     C     P5RECD        LOOKUP    @RECD                                  77
     C                   IF        *IN77 =  *OFF
     C                   EVAL      J = J + 1
     C                   MOVE      P5RECD        @RECD(J)
     C                   SORTA     @RECD
     C                   ENDIF
     CL1                 IF        J > 1
     CL1                 EXSR      DONTFIX
     CL1                 ENDIF
     C**********************************************************************
     C* OMIT CLAIMS WITH MULT RSV CODES
     C**********************************************************************
     C     DONTFIX       BEGSR

     C     PV2KY         SETLL     BADPV2L
     C     PV2KY         READE     BADPV2L
     C                   DOW       NOT %EOF(BADPV2L)
     C                   MOVEL(P)  'DONTFIX'     xPARQST
     C                   UPDATE    BADPVR
     C     PV2KY         READE     BADPV2L
     C                   ENDDO

     C     PV2KY         SETLL     BADPV2RL
     C     PV2KY         READE     BADPV2RL
     C                   DOW       NOT %EOF(BADPV2RL)
     C                   MOVEL(P)  'DONTFIX'     xRPRQST
     C                   UPDATE    BADPVRR
     C     PV2KY         READE     BADPV2RL
     C                   ENDDO
     C                   ENDSR
