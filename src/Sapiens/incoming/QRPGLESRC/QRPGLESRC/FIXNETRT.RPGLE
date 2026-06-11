     H* (c) 2010 StoneRiver, Inc. All rights reserved.
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE FIXNETRT - Populate Net Rates Fix Program
     H* CREATED: 09/14/10 CONSULTANT: C:257 PROJECT: 93WJC
     H*****************************************************************
     H* REVISIONS: XXXXXX                                             *
     H*****************************************************************
     F*
     FWDMPL     IF   E           K DISK
     F* Employer Premium File <MP>
     F*
     FWDELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     F* CO#/FND/FYR/EMP#/DIV <EL>
     F*
     FWMEPL1E   UF   E           K DISK
     F                                     RENAME(PEP01:PEP02)
     F* CO#/FND/EMP#/DIV/FYR/EFDT <EP>
     F*
     FWMRSL     IF   E           K DISK
     F* WORKERS' COMP RATE SET CODE MASTER FILE <RS>
     C*
     C*KLIST
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*                  KFLD                    XXDIV
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    RSRTCD        XXRTCD
     C     *LIKE         DEFINE    RSJRST        XXST
     C**********************************************************************************************
     C* Main Logic
     C**********************************************************************************************
     C     *LOVAL        SETLL     PEL02                                        WDELL1
     C                   READ      PEL02                                  60    WDELL1
     C                   dow       *IN60 = *OFF
     C                   IF        ELFAF <> *BLANKS
     C                   CLEAR                   XXTMAN           11 2          TOTAL AUD MAN PREM
     C                   CLEAR                   XXANMP           11 2          TOTAL AUD NORM PREM
     C                   CLEAR                   XXENMP           11 2          TOTL NORMAL PREM
     C                   CLEAR                   XXMAPR           11 2          TOTL MANUAL PREMIUM
     C                   z-add     ELCO#         XXCO#
     C                   z-add     ELFND         XXFND
     C                   z-add     ELFYR         XXFYR
     C                   z-add     ELEMP#        XXEMP#
     C                   z-add     ELDIV         XXDIV
     C                   movel     ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60        WMRSL
     C                   if        *in60 = *off
     C                   MOVE      RSJRST        XXST
     C*
     C     ELKY          CHAIN     PMP01                              60        WDMPL
     C     *IN60         DOWEQ     '0'
     C*
     C                   IF        MPST = XXST
     C                   ADD       MPAMPR        XXTMAN                         AUD MAN PREM
     C                   ADD       MPANMP        XXANMP                         AUD NORM PREM
     C                   ADD       MPENMP        XXENMP                         NORMAL PREM
     C                   ADD       MPMAPR        XXMAPR                         MANUAL PREMIUM
     C                   ENDIF
     C*
     C     ELKY          READE     PMP01                                  60    WDMPL
     C                   ENDDO                                                  *IN60 DOWEQ
     C* Calculate the estimated net rate factor , by dividing the total normal premium
     C* by the total manual premium.  Set the net rate factor equal to 1 if total manual premium is
     C* zero.
     C*
     C     XXMAPR        IFNE      *ZERO                                        TOTAL MAN PREM
     C     XXENMP        DIV(H)    XXMAPR        XXNETR            9 6          EST NET RATE FACTOR
     C                   ELSE
     C                   Z-ADD     1             XXNETR                         EST NET RATE FACTOR
     C                   ENDIF
     C*
     C*Calculate the audited net rate factor , by dividing the total audited normal
     C*premium by the total audited manual premium.  Set the net rate factor equal to 0 if total
     C*audited manual premium is zero and ELFAF is not equal to 'A', otherwise set the net rate
     C*factor to 1 if total audited manual premium is zero.
     C*
     C                   select
     C     XXTMAN        WHENEQ    *zeros
     C     ELFAF         ANDNE     'A'
     C                   Z-ADD     0             XXANETR           9 6          AUD NET RATE FACTOR
     C     XXTMAN        WHENEQ    *zeros
     C                   Z-ADD     1             XXANETR                        AUD NET RATE FACTOR
     C     XXTMAN        WHENNE    *zeros
     C     XXANMP        DIV(H)    XXTMAN        XXANETR                        AUD NET RATE FACTOR
     C                   ENDSL
     C*
     C     ELKY          CHAIN     PEP02                              60        WMEPL1E
     C     *IN60         DOWEQ     '0'
     C* If the payroll estimated net rate field is zero (EPENTR)
     C* Apply the estimated net rate factor against the EPARAT rate and EPEMPR premium fields to
     C* populate into the estimated net rate EPENTR and estimated net premium EPENTP.
     C*
     C                   Z-ADD     0             XXWK10           11 0
     C                   IF        EPENTR = *ZERO
     C     XXNETR        MULT(H)   EPARAT        EPENTR
     C     XXNETR        MULT(H)   EPEMPR        XXWK10
     C                   Z-ADD     XXWK10        EPENTP
     C                   ENDIF
     C* If the payroll audited net rate field is zero (EPANTR)
     C* Apply the audited net rate factor against the EPARAT rate and EPAMPR premium fields to
     C* populate into the audited net rate EPANTR and audited net premium EPANTP.
     C*
     C                   Z-ADD     0             XXWK10
     C                   IF        EPANTR = *ZERO
     C     XXANETR       MULT(H)   EPARAT        EPANTR
     C     XXANETR       MULT(H)   EPAMPR        XXWK10
     C                   Z-ADD     XXWK10        EPANTP
     C                   ENDIF
     C*
     C                   UPDATE    PEP02                                        WMEPL1E
     C     ELKY          READE     PEP02                                  60    WMEPL1E
     C                   ENDDO                                                  *IN60 DOWEQ PEP02
     C                   endif                                                  xxst
     C                   endif                                                  "Y"
     C                   READ      PEL02                                  60    WDELL1
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   SETON                                            LR
     C*
     C*
