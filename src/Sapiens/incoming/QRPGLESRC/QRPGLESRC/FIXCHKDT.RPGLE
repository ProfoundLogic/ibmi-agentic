     H*  (C) Copyright Fiserv, Inc., 2007
     H******************************************************************
     H*/TITLE FIXCHKDT - Update transaction/request date               *
     H******************************************************************
     H*  DESCRIPTION: This program will update the transaction date and*
     H*               request entry date on payment history records >= *
     H*               09/01/2006                                       *
     H*                                                                *
     H*  CREATED: 04/20/07  CONSULTANT: C:191  PROJECT: 139WGD         *
     H******************************************************************
     H*                                                                *
     H* REVISIONS:                                                     *
     H*                                                                *
     H******************************************************************
     F* Claims checks payment file
     FWDPAL9    UF   E           K DISK
     F*
     F* Request payment, monthly transactions
     FWARPPML1  IF   E           K DISK
     F*
     C*************************************************************************
     C*
     C* Parameter List
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C                   PARM                    ##OUTQ           10
     C                   PARM                    ##XCO#            1
     C                   PARM                    ##XFND            1
     C                   PARM                    ##XEMP            1
     C                   PARM                    ##XDIV            1
     C*
     C*  Key list
     C* Checks Payment File
     C     PAKY          KLIST
     C                   KFLD                    xxCO#             3 0
     C                   KFLD                    xxTRDT            7 0
     C*
     C* Request payment, monthly transactions
     C     ARKY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C                   KFLD                    PACHK#
     C*
     C*====================================================================
     C*  MAIN
     C*====================================================================
     C* Get Company#
     C                   CALL      'WA001R'      COMPL
     C* Load PAKY key field values
     C                   EVAL      xxCO# = ##CO#
     C                   EVAL      xxTRDT = 1060901
     C*
     C* Set lower limit on WDPAL9
     C     PAKY          SETLL     PPA01
     C     xxCO#         READE     PPA01
     C                   DOW       NOT %EOF
     C* Leave processing if transaction date is 1070320
     C                   IF        PATRDT >= 1070320
     C                   LEAVE
     C                   ENDIF                                                  PATRDT >= 1070320
     C*
     C* Update record if Record Type = 'P' and Adjustment Status = ' '
     C                   IF        PATYPE = 'P' AND PAADST = *BLANKS
     C                   EXSR      SRUpdatePA
     C                   ENDIF                                                  PATYPE = 'P'
     C*
     C     xxCO#         READE     PPA01
     C                   ENDDO                                                  NOT %EOF
     C*
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C*====================================================================
     C*  SRUpdatePA - Update Claims checks payment file
     C*====================================================================
     C     SRUpdatePA    BEGSR
     C*
     C                   EVAL      PARQDT = *ZEROS
     C* Set Transaction date = Date Maintained
     C                   EVAL      PATRDT = PADT
     C* Set Initial Request Date = date the payment was entered into the system
     C     ARKY          CHAIN     PRP01
     C                   IF        %FOUND
     C                   EVAL      PARQDT = RPIDT
     C                   ENDIF                                                  %FOUND
     C* Update Claims checks payemnt file record
     C                   UPDATE    PPA01
     C                   ENDSR                                                  SRUpdate
