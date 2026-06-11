     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* ***************************************************************
     H/TITLE W7269R - EMPLOYER/CLAIM TRANSFER ACROSS GROUPS
     H Bnddir('WTBNDDIR')
     H* ***************************************************************
     H*  DESCRIPTION:  This program allows an employer to be          *
     H*                Transferred from one group to another.  The    *
     H*                data will include all employer data, claims,   *
     H*                deductibles, payments and DARTS                *
     H*                                                               *
     H*                The data is being transferred to another group,*
     H*                the employer and claim numbers will most likely*
     H*                be changed.  To keep track of the changes, a   *
     H*                file will hold the old and new employer and    *
     H*                claim numbers.                                 *
     H*                                                               *
     H*                                                               *
     H*  CALLED FROM: EMPLOYER PROCESS MENU                           *
     H*                                                               *
     H*  CALLS TO: WA036R   - WCMS Employer Transfer API              *
     H*            WA036R01 - WCMS Employer Transfer API (add'l files)*
     H*            WA037R   - WCMS Claim Transfer API                 *
     H*            WA800R   - WCMC Employer Transfer API              *
     H*            FA800R01 - WCMC Claim Transfer API                 *
     H*                                                               *
     H*  CREATED: 05/21/99  PROGRAMMER: C:112 PROJECT: 65WDC          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 12/26/24 C:062 Support: 118105(150)                           *
     H*                Monitor the managed care programs.             *
     H*                                                               *
     H* 05/23/24 C:300 Support: 115634 (SUNZ)                         *
     H*                Update WMFNP Maintenance fields while updating *
     H*                Next Available Employer Number                 *
     H*                                                               *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     H* 06/03/11 C:65  Support: SUPP#WA139/77007                      *
     H*                PREVENT SEARCH ON FROM EMPLOYER # SINCE IT     *
     H*                IS ALWAYS A PROTECTED FIELD.                   *
     H*                                                               *
     H* 01/27/10 C:62  Support: WC135/71230                           *
     H*                Transfer Darts if group is different.          *
     H*                                                               *
     H* 01/03/08 C:15  Support: WC135/60772                           *
     H*                Chgd code to not call W8024R if a claim is     *
     H*                transferred to another emp/div within the same *
     H*                PEO, for clt135.                               *
     H*                                                               *
     H*  01/09/04 C:53  Project: 128WBD                               *
     H*                 Group transfer change                         *
     H*                                                               *
     H*  11/17/03 C:53  Project: 126WBP                               *
     H*                 Group transfer                                *
     H*                                                               *
     H*  12/05/01 C:15  Support: WC065/29203                          *
     H*                 Added code to correctly assign the employer # *
     H*                 if the Group auto assign flag = 'N'.          *
     H*                                                               *
     H*  11/15/01 C:053 Project: 114AAB                               *
     H*                 Add WCMC files                                *
     H*                                                               *
     H*  02/15/00 C:112 Project: 65WDT                                *
     H*                 Added code to allow transfer of a claim from  *
     H*                 one group to another.                         *
     H*                                                               *
     H*****************************************************************
     FW7269F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
      *  Fund Master file
     F*
     FWMFNL     UF   E           K DISK
     F*  Logical file definition Employer Master File
     FWMEMP     UF   E             DISK
     F                                     RENAME(PEM01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWMEML     IF   E           K DISK
     F                                     RENAME(PEM01:PEM02)
      *
     FWMCML     IF   E           K DISK
      *** BEGIN ADD *** 02/15/00 **************************************
     FWMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FWMCMP     IF   E             DISK
     F                                     RENAME(PCM01:PCM03)
     FWDF1L     IF   E           K DISK
     FWDF1L1    IF   E           K DISK
     F                                     RENAME(PF101:PF102)
     FWDF2L     IF   E           K DISK
     FWMALL     IF   E           K DISK
     FWMOOL2    IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWDELL     UF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     FWDELL2    IF   E           K DISK
     FWMCXL     IF   E           K DISK
     FWDDDL     IF   E           K DISK
      *** END ADD ***** 02/15/00 **************************************
     F*  Logical file definition for WCMS Employer Transfer File
     FWAETL     O    E             DISK
      /COPY SCOPYMT01D
      ***  Begin Add  ***  06/09/20  ****************************
      /copy sPrMask
      /copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
     D*** BEGIN ADD *** 02/15/00 **************************************
     D*** END ADD ***** 02/15/00 **************************************
     D*
     D***PY SCOPYMT01I
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D*** BEGIN ADD *** 02/15/00 **************************************
     D                 DS
     D  ED                     1     35  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D                 DS
     D  CD                     1     35  0
     D                                     DIM(5) ASCEND                        CANCEL DATE
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D                 DS
     D  RR                     1     90  2
     D                                     DIM(10)                              Reserves
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D                 DS
     D  AR                     1     90  2
     D                                     DIM(10)                              Total paid
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D                 DS
     D  OOOSS#                 1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
      *** END ADD ***** 02/15/00 **************************************
      *
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Employer/claim Trans-
     D                                     fer Across Groups')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*   Key list for the Claim Master file
     C*
     C     CMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C*** BEGIN ADD *** 02/15/00 **************************************
     C     CMKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*** END ADD ***** 02/15/00 **************************************
     C*
     C*   Key list for the Group Master File
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXFND
     C*
     C*   Key list for the Employer Master file
     C*
     C     EMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C*   Key list for the Employer Master file
     C*
     C*** BEGIN ADD *** 02/15/00 **************************************
     C     EMKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  KEY LIST FOR EMPLOYER LOCATION FILE
     C*
     C     ALKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    CMCTG1
     C*
     C*  DEDUCTIBLE DETAIL BILLS FILE
     C*
     C     DDKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     ELKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXEFDT
     C*
     C     ELKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*   Key list for the Group Master File
     C*
     C     F1KY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C     F1KY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEFDT
     C*
     C*   KEY LIST FOR OWNERS / OFFICERS FILE
     C*
     C     OOKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    CMCTG1
     C                   KFLD                    CMSS#
     C                   KFLD                    CMACDT
     C*
     C     OOKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    CMCTG1
     C                   KFLD                    CMSS#
      *** END ADD ***** 02/15/00 **************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    EMFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    FNNACS        XXCASE
      *** BEGIN ADD *** 02/15/00 **************************************
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMFYR         XXTFYR
     C     *LIKE         DEFINE    CMCASE        XXTCAS
     C     *LIKE         DEFINE    EMEMP#        XXTEMP
     C     *LIKE         DEFINE    ELPFDT        XXEFDT
     C     *LIKE         DEFINE    DDAMTB        XXAMTB
     C     *LIKE         DEFINE    ELOUTS        XXRR
     C     *LIKE         DEFINE    ELTCLP        XXAR
      *** END ADD ***** 02/15/00 **************************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*** BEGIN ADD *** 02/15/00 **************************************
     C*================================================================
     C* CLMTOT - Update the claim totals in the Emp detail file.
     C*================================================================
     C     CLMTOT        BEGSR
     C*
     C*  First, accumulate the claim's reserves and payments
     C*
     C                   XFOOT     RR            XXRR
     C                   XFOOT     AR            XXAR
     C*
     C*  Subtract totals from the 'from' employer
     C*
     C                   MOVE      SCFFND        XXFND
     C                   MOVE      CMEMP#        XXEMP#
     C                   MOVE      CMDIV         XXDIV
     C                   MOVE      SCFYR         XXFYR
     C     ELKY2         CHAIN     PEL02                              99
     C     *IN99         IFEQ      *OFF
     C                   SUB       XXRR          ELOUTS
     C                   SUB       XXAR          ELTCLP
     C                   SUB       1             ELT#CL
     C     CMSTAT        IFEQ      'C'
     C                   SUB       1             ELTCLC
     C                   ENDIF
     C                   UPDATE    PEL02
     C                   ENDIF
     C*
     C*  Add totals to the 'to' employer
     C*
     C                   MOVE      SCTFND        XXFND
     C                   MOVE      SCTEMP        XXEMP#
     C                   MOVE      SCTDIV        XXDIV
     C                   MOVE      XXTFYR        XXFYR
     C     ELKY2         CHAIN     PEL02                              99
     C     *IN99         IFEQ      *OFF
     C                   ADD       XXRR          ELOUTS
     C                   ADD       XXAR          ELTCLP
     C                   ADD       1             ELT#CL
     C     CMSTAT        IFEQ      'C'
     C                   ADD       1             ELTCLC
     C                   ENDIF
     C                   UPDATE    PEL02
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END ADD ***** 02/15/00 **************************************
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C                   MOVEA     '00000'       *IN(21)
     C                   DO
     C*
     C* Validate the 'From' group
     C*
     C     SCFFND        IFLE      *ZERO
     C                   MOVEA     '0001'        *IN(80)
     C                   SETON                                        90
     C                   LEAVE
     C                   ENDIF
     C*
     C                   MOVEL     SCFFND        XXFND
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEA     '0010'        *IN(80)
     C                   SETON                                        90
     C                   LEAVE
     C                   ENDIF
     C*
     C* Validate the 'From' employer
     C*
     C     SCEMP#        IFNE      *ZERO
     C*
     C                   MOVE      SCEMP#        XXEMP#
     C     EMKY          CHAIN     PEM02                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEA     '0011'        *IN(80)
     C                   SETON                                        90
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 02/15/00 **************************************
     C* Validate the 'From' claim
     C*
     C     SCFYR         IFNE      *ZERO
     C     SCCASE        ORNE      *ZERO
     C*
     C                   MOVE      SCFYR         XXFYR
     C                   MOVE      SCCASE        XXCASE
     C     CMKY1         CHAIN     PCM02                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEA     '0100'        *IN(80)
     C                   SETON                                        90
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* The 'From' employer and claim are mutually exclusive.
     C*
     C     SCEMP#        IFNE      0
     C     SCCASE        ANDNE     0
     C                   MOVEA     '0101'        *IN(80)
     C                   SETON                                        90
     C                   LEAVE
     C                   ENDIF
     C*** END ADD ***** 02/15/00 **************************************
     C*
     C* Validate the 'To' group
     C*
     C     SCTFND        IFLE      *ZERO
     C                   MOVEA     '0110'        *IN(80)
     C                   SETON                                        90
     C                   LEAVE
     C                   ENDIF
     C*
     C                   MOVEL     SCTFND        XXFND
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEA     '0111'        *IN(80)
     C                   SETON                                        90
     C                   LEAVE
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 02/15/00 **************************************
     C* Validate the 'To employer' being transferred to a new group
     C*
     C     SCTEMP        IFNE      *ZERO
     C*
     C                   MOVE      SCTEMP        XXEMP#
     C                   MOVE      SCTDIV        XXDIV
     C     EMKY1         CHAIN     PEM02                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEA     '1000'        *IN(80)
     C                   SETON                                        90
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* The 'From' claim and 'To' employer are mutually inclusive.
     C*
     C     SCCASE        IFNE      0
     C     SCTEMP        ANDEQ     0
     C     SCCASE        OREQ      0
     C     SCTEMP        ANDNE     0
     C                   MOVEA     '1001'        *IN(80)
     C                   SETON                                        90
     C                   LEAVE
     C                   ENDIF
     C*
     C* Verify that either the 'From' employer field is entered, or
     C*   the 'From' claim and 'To' employer fields are entered.
     C*
     C     SCEMP#        IFEQ      *ZERO
     C     SCCASE        ANDEQ     *ZERO
     C     SCTEMP        ANDEQ     *ZERO
     C                   MOVEA     '1010'        *IN(80)
     C                   SETON                                        90
     C                   LEAVE
     C                   ENDIF
     C*
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*  If transferring a claim, calculate the new claim FYR, then
     C*    process emp coverage edits, if appropriate.
     C*
B01  C     SCCASE        IFNE      *ZERO
     C*
     C                   Z-ADD     SCTFND        XXFND
     C                   Z-ADD     SCTEMP        XXEMP#
     C                   Z-ADD     SCTDIV        XXDIV
     C                   Z-ADD     CMACDT        XXEFDT
     C*
     C     ELKY1         SETGT     PEL01
     C     ELKY          READPE    PEL01                                  99
     C     *IN99         IFEQ      *OFF
     C     ELPTDT        ANDGT     CMACDT
     C                   Z-ADD     ELFYR         XXTFYR
     C                   ELSE
     C                   MOVE      *ON           *IN21                            Policy year invali
     C                   ENDIF
     C*
     C*  If there was no effective emp policy, get FYR from Group Detail.
     C*
     C     *IN21         IFEQ      *ON
     C*
     C     F1KY1         SETGT     PF102
     C     FNKY          READPE    PF102                                  99
     C     *IN99         IFEQ      *OFF
     C     F1ENPO        ANDGT     CMACDT
     C                   Z-ADD     F1FYR         XXTFYR
     C                   ELSE
     C     CMACDT        DIV       10000         XXTFYR
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  Determine if coverage edits should be processed.
     C*
     C                   Z-ADD     XXTFYR        XXFYR
     C     F1KY          CHAIN     PF101                              99
     C   99              MOVE      *BLANK        F1RLXC
     C*
B02  C     F1RLXC        IFNE      'Y'                                           process cov edits
     C*
     C*----------
     C*   Check the employer eff and canc dates
     C*
     C                   Z-ADD     1             Y                 3 0
     C     *ZERO         LOOKUP    ED(Y)                                  99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     5             Y
     C                   ELSE
     C                   SUB       1             Y
     C                   END
     C*
     C     Y             DOWNE     0
     C     CMACDT        IFGE      ED(Y)
     C                   LEAVE
     C                   ELSE
     C                   SUB       1             Y
     C                   END
     C                   ENDDO
     C*
     C*  The accident date can't be greater than the cancel date,
     C*    unless overriden by the user.
     C*
     C     Y             IFEQ      *ZERO
     C                   Z-ADD     1             Y
     C                   MOVE      *ON           *IN22
     C                   ELSE
     C*
     C     CD(Y)         IFLE      CMACDT
     C     CD(Y)         ANDGT     *ZERO
     C                   MOVE      *ON           *IN22
     C                   END
     C*
     C                   END
     C*
     C*----------
     C*  If the accident date is greater than the location deletion
     C*    date, send error or allow user to override (if authorized).
     C*
     C     CMCTG1        IFNE      *BLANK
     C*
     C     ALKY          CHAIN     PAL01                              99
     C     *IN99         IFEQ      *OFF
     C     ALDLDT        ANDNE     0
     C     ALDLDT        ANDLE     CMACDT
     C***                  MOVE *ON       *IN23
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*----------
     C*  If the claimant is an owner, determine if owner is excluded
     C*    from coverage.
     C*
     C     OOKY          SETGT     POO01
     C     OOKY1         READPE    POO01                                  99
     C     *IN99         IFEQ      *OFF
     C*
     C     OOEXFL        IFEQ      'E'                                          EXCLUDED
     C                   SETON                                        24
     C                   ENDIF
     C*
     C     OOEXFL        IFNE      'E'                                          INCLUDED
     C     OOEXDT        ANDNE     0
     C     OOEXDT        ANDLE     CMACDT
     C                   SETON                                        25
     C                   ENDIF
     C*
     C     *IN24         IFEQ      *ON
     C     *IN25         OREQ      *ON
     C*
     C     OOOFSN        CAT(P)    OOOLSN:1      SCOONM
      *****  Begin Chg  ***  06/09/20  **********************
     C*****SS1           CAT(P)    '-':0         SCOOSS
     C*****SCOOSS        CAT       SS2:0         SCOOSS
     C*****SCOOSS        CAT       '-':0         SCOOSS
     C*****SCOOSS        CAT       SS3:0         SCOOSS
      /Free
       scOOSS = DisplaySS#(MaskData('SSN':'OOOSS#':OOOSS#));
      /End-Free
      *****  End   Chg  ***  06/09/20  **********************
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                           CYMD --> MDCY
     C                   MOVEL(P)  SRDTO         SCACDT
     C*
     C                   Z-ADD     OOEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                           CYMD --> MDCY
     C                   MOVEL(P)  SRDTO         SCEFDT
     C*
     C                   Z-ADD     OOEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                           CYMD --> MDCY
     C                   MOVEL(P)  SRDTO         SCEXDT
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*----------
     C*  If any of the coverage edits were not passed, either
     C*    1) allow the user to override the edit if they have
     C*       adequate authority, or
     C*    2) display an error msg.
     C*
B03  C     *IN21         IFEQ      *ON
     C     *IN22         OREQ      *ON
     C     *IN23         OREQ      *ON
     C     *IN24         OREQ      *ON
     C     *IN25         OREQ      *ON
     C*
     C*  Determine if user has authority to override error.
     C*
     C                   MOVEL     SCTFND        XXFND
     C     FNKY          CHAIN     PCX01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVE      UPLVL         SRULVL
     C                   MOVE      CXSECC        SRCLVL
     C                   CALL      'SRCCSC'
     C                   PARM      UPTYPE        SRUTYP            1
     C                   PARM                    SRULVL            3
     C                   PARM      CXOCAT        SRCTYP            1
     C                   PARM                    SRCLVL            3
     C                   PARM      '1'           SREXEC            1
     C                   PARM                    SRRES             1
     C                   PARM      CXOCA2        SRCTP2            1
     C                   PARM      CXOCA3        SRCTP3            1
     C                   PARM      CXOCA4        SRCTP4            1
     C                   PARM      CXOCA5        SRCTP5            1
     C                   PARM      ' '           SRCTP6            1
     C*
     C                   ENDIF
     C*
B04  C     *IN99         IFEQ      *ON                                           user does not have
     C     SRRES         OREQ      '1'                                             authority
     C                   SETON                                          90
     C                   LEAVE
     C*
B04  C                   ELSE
     C*
     C*  User has authority, display appropriate override window.
     C*
     C*  -- no employer coverage --
     C*
     C     *IN21         IFEQ      *ON
     C     *IN22         OREQ      *ON
     C*
     C                   MOVE      'N'           SCEOV1
     C     RDSP1         TAG
     C                   EXFMT     WDW01
     C     SCEOV1        IFNE      'Y'
     C     SCEOV1        ANDNE     'N'
     C                   SETON                                          85
     C                   GOTO      RDSP1
     C                   ENDIF
     C*
     C     SCEOV1        IFNE      'Y'
     C                   SETON                                          90
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  -- location deleted --
     C*
     C     *IN23         IFEQ      *ON
     C*
     C                   MOVE      'N'           SCEOV1
     C     RDSP2         TAG
     C                   EXFMT     WDW02
     C     SCEOV1        IFNE      'Y'
     C     SCEOV1        ANDNE     'N'
     C                   SETON                                          85
     C                   GOTO      RDSP2
     C                   ENDIF
     C*
     C     SCEOV1        IFNE      'Y'
     C                   SETON                                          90
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  -- owner excluded from coverage --
     C*
     C     *IN24         IFEQ      *ON
     C     *IN25         OREQ      *ON
     C*
     C                   MOVE      'N'           SCEOV1
     C     RDSP3         TAG
     C                   EXFMT     WDW03
     C     SCEOV1        IFNE      'Y'
     C     SCEOV1        ANDNE     'N'
     C                   SETON                                          85
     C                   GOTO      RDSP3
     C                   ENDIF
     C*
     C     SCEOV1        IFNE      'Y'
     C                   SETON                                          90
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*
E04  C                   ENDIF
     C*----------
E03  C                   ENDIF
E02  C                   ENDIF
E01  C                   ENDIF
     C*
     C*---------------------------------------------------------------------
     C*
     C                   ENDDO
     C*
     C*  All error checks have passed at this point.  This following
     C*  code sets a flag to indicate whether it is an employer
     C*  transfer from one group to another (TRNXFL = 'E') or a single
     C*  claim transfer from one group to another (TRNXFL = 'C').
     C*
     C                   MOVE      *BLANK        TRNXFL            1
     C     SCEMP#        IFNE      *ZERO
     C                   MOVE      'E'           TRNXFL
     C                   ELSE
     C     SCFYR         IFNE      *ZERO
     C     SCCASE        ANDNE     *ZERO
     C                   MOVE      'C'           TRNXFL
     C                   ENDIF
     C                   ENDIF
     C*** END ADD ******** 02/15/00 ***********************************
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
      *===============================================================
      * KJDO - Subroutine for F10=Table search
      *===============================================================
     C     KJDO          BEGSR
      *
     C                   SELECT
      *
      *  Select a valid 'From' Group
      *
     C     HFLD          WHENEQ    'SCFFND'
     C                   CALL      'W0010R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@CO#             3
     C                   PARM      *ZERO         @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM      1             @@VIEW            1 0
      *
     C     @@FND         IFNE      *ZERO
     C                   MOVE      @@FND         SCFFND
     C                   SETON                                        7190
     C                   ELSE
     C                   SETON                                          90
     C                   ENDIF
      *
      *  Select a valid 'From' Employer
      *
     C     HFLD          WHENEQ    'SCEMP#'
     C*** BEGIN ADD ***  06/03/11  *********************************************
     C     HFLD          ANDNE     'SCEMP#'                                     DISABLE SEARCH
     C*** END ADD *****  06/03/11  *********************************************
      *
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @@KC
     C     *INKL         PARM                    @@KL
     C                   PARM      '2'           @@CALL
     C                   PARM      *ZEROS        @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C                   PARM                    @@QTE             1
      *
     C     @@RRN         IFNE      *ZERO
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C                   MOVE      EMFND         SCFFND
     C                   MOVE      EMEMP#        SCEMP#
     C                   SETON                                        7390
     C                   ELSE
     C                   SETON                                        7190
     C                   ENDIF
      *
      *** BEGIN ADD *** 02/15/00 **************************************
      *  Select a valid claim
      *
     C     HFLD          WHENEQ    'SCCASE'
     C     HFLD          OREQ      'SCFYR'
      *
     C                   CALL      'W0070R'
     C                   PARM      '2'           @@CALL
     C     *INKC         PARM                    @@KC
     C     *INKL         PARM                    @@KL
     C                   PARM      *BLANK        XXMODE            1
     C                   PARM      *ZEROS        @@RRN             9 0
     C                   PARM      'CN'          @@SORT            2
     C                   PARM      *ZEROS        @@RRN2            9 0
     C                   PARM                    @@YEAR            3 0
      *
     C     @@RRN         IFNE      *ZERO
     C     @@RRN         CHAIN(N)  PCM03                              99
     C                   MOVE      CMFND         SCFFND
     C                   MOVE      CMFYR         SCFYR
     C                   MOVE      CMCASE        SCCASE
     C                   SETON                                        7290
     C                   ELSE
     C                   SETON                                        7390
     C                   ENDIF
      *** END ADD ***** 02/15/00 **************************************
      *
      *  Select a valid 'To' Group
      *
     C     HFLD          WHENEQ    'SCTFND'
      *
     C                   CALL      'W0010R'
     C     *INKC         PARM                    @@KC
     C     *INKL         PARM                    @@KL
     C                   PARM      '2'           @@CALL
     C                   PARM                    @@CO#
     C                   PARM      *ZERO         @@FND
     C                   PARM                    @@FYR
     C                   PARM      1             @@VIEW
     C*
     C     @@FND         IFGT      *ZERO
     C                   MOVEL     @@FND         SCTFND
     C                   SETON                                        7490
     C                   ELSE
     C                   SETON                                        7290
     C                   ENDIF
      *
      *** BEGIN ADD *** 02/15/00 **************************************
      *  Select a valid 'To' Employer
      *
     C     HFLD          WHENEQ    'SCTEMP'
      *
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @@KC
     C     *INKL         PARM                    @@KL
     C                   PARM      '2'           @@CALL
     C                   PARM      *ZEROS        @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C*
     C     @@RRN         IFNE      *ZERO
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C                   MOVE      EMFND         SCTFND
     C                   MOVE      EMEMP#        SCTEMP
     C                   MOVE      EMDIV         SCTDIV
     C                   SETON                                          90
     C                   ELSE
     C                   SETON                                        7490
     C                   ENDIF
      *** END ADD ***** 02/15/00 **************************************
      *
     C                   ENDSL
      *
     C                   ENDSR
      *** BEGIN ADD *** 02/15/00 **************************************
     C*================================================================
     C* MVCLM - Transfer a single claim from one Group/Emp to another
     C*================================================================
     C     MVCLM         BEGSR
     C*
     C*** Begin add *** 01/03/08 **************************************
     C     SCFFND        IFNE      SCTFND
     C*** End add ***** 01/03/08 **************************************
     C*
     C*  Get the 'From' and 'To' bank account numbers.
     C*
     C*        'From' bank account
     C*
     C                   Z-ADD     SCFFND        XXFND
     C     FNKY          CHAIN     PF201                              60
     C  N60              MOVE      F2CTYP        QQCALL
     C*
     C                   MOVE      ##CO#         QQCO#
     C                   MOVE      SCFFND        QQFND
     C                   MOVE      DSDATE        QQSRDT
     C                   CALL      'WA023R'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM      'WC'          QQAPPL            2
     C                   PARM                    QQSRDT            8
     C                   PARM                    QQACCT           20            Bank Acct #
     C                   PARM                    QQCALL            1            (V)oucher/(C)heck
     C                   PARM      *BLANK        QQHDCY            1
     C                   PARM      *BLANK        QQCHKQ           10
     C                   PARM      *BLANK        QQFTYP           10
     C                   PARM      *BLANK        QQRTCD            1
     C                   PARM      *ZERO         QQEFDT            7
      *
     C     QQRTCD        IFEQ      *BLANKS
     C                   MOVE      QQACCT        ETOACT
     C                   ELSE
     C                   MOVE      *BLANKS       ETOACT
     C                   ENDIF
     C*
     C*        'To' bank account
     C*
     C                   Z-ADD     SCTFND        XXFND
     C     FNKY          CHAIN     PF201                              60
     C  N60              MOVE      F2CTYP        QQCALL
     C*
     C                   MOVE      SCTFND        QQFND
     C                   MOVE      DSDATE        QQSRDT
     C                   CALL      'WA023R'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM      'WC'          QQAPPL
     C                   PARM                    QQSRDT
     C                   PARM                    QQACCT                         Bank Acct #
     C                   PARM                    QQCALL                         (V)oucher/(C)heck
     C                   PARM      *BLANK        QQHDCY
     C                   PARM      *BLANK        QQCHKQ
     C                   PARM      *BLANK        QQFTYP
     C                   PARM      *BLANK        QQRTCD
     C                   PARM      *ZERO         QQEFDT
      *
     C     QQRTCD        IFEQ      *BLANKS
     C                   MOVE      QQACCT        ETACCT
     C                   ELSE
     C                   MOVE      *BLANKS       ETACCT
     C                   ENDIF
     C*
     C*  Retrieve the next available claim #
     C*
     C                   Z-ADD     1             FNNACS
     C                   Z-ADD     SCTFND        XXFND
     C     FNKY          CHAIN     PFN01                              99
     C                   ADD       1             FNNACS
      *** Beg Add ***** 05/23/24 ***********************
     C                   MOVE      Q1USER        FNUSER
     C                   MOVE      Q1PGM         FNPGM
     C                   Z-ADD     SAVEDT        FNDT
     C                   MOVE      DSTIME        FNTIME
      *** End Add ***** 05/23/24 ***********************
     C  N99              UPDATE    PFN01
     C  N99              FEOD      WMFNL
     C     FNNACS        SUB       1             XXTCAS
     C*
     C*** Begin add *** 01/03/08 **************************************
     C                   ELSE
     C                   MOVE      SCCASE        XXTCAS
     C                   ENDIF
     C*** End add ***** 01/03/08 **************************************
     C*
     C* Write record to WCMS Employer Transfer File
     C*
     C                   MOVE      ##CO#         ETCO#
     C                   MOVE      SCTFND        ETFND
     C                   MOVE      SCTEMP        ETEMP#
     C                   MOVE      SCTDIV        ETDIV
     C                   MOVE      XXTFYR        ETFYR
     C                   MOVE      XXTCAS        ETCASE
     C                   MOVE      SCFFND        ETOFND
     C                   MOVE      CMEMP#        ETOEMP
     C                   MOVE      CMDIV         ETODIV
     C                   MOVE      SCFYR         ETOFYR
     C                   MOVE      SCCASE        ETOCAS
     C*
     C                   MOVE      Q1USER        ETUSER
     C                   MOVE      Q1PGM         ETPGM
     C                   Z-ADD     SAVEDT        ETDT
     C                   MOVE      DSTIME        ETTIME
     C                   MOVE      Q1USER        ETIUSR
     C                   MOVE      Q1PGM         ETIPGM
     C                   Z-ADD     SAVEDT        ETIDT
     C                   MOVE      DSTIME        ETITIM
     C*
     C                   WRITE     PET01
     C*
     C*  Call W8024R to reverse all associated deduct entries.
     C*
     C*** Begin add *** 01/03/08 **************************************
     C     CLTID         IFNE      '135'
     C     FNPEOF        OREQ      *BLANKS
     C     SCFFND        ORNE      SCTFND
     C*** End add ***** 01/03/08 **************************************
     C*
     C                   EXSR      RVDED
     C*
     C*** Begin add *** 01/03/08 **************************************
     C                   ENDIF
     C*** End add ***** 01/03/08 **************************************
     C*
     C*  call API to update all WCMS claim files
     C*
     C                   MOVE      SCFFND        @@FGRP
     C                   MOVE      CMEMP#        @@FEMP
     C                   MOVE      CMDIV         @@FDIV
     C                   MOVE      SCFYR         @@FFYR
     C                   MOVE      SCCASE        @@FCAS
     C                   MOVE      SCTFND        @@TGRP
     C                   MOVE      SCTEMP        @@TEMP
     C                   MOVE      SCTDIV        @@TDIV
     C                   MOVE      XXTFYR        @@TFYR
     C                   MOVE      XXTCAS        @@TCAS
      *
     C                   CALL      'WA037R'
     C                   PARM                    @@CO#
     C                   PARM                    @@FGRP
     C                   PARM                    @@FEMP
     C                   PARM                    @@FDIV
     C                   PARM                    @@FFYR
     C                   PARM                    @@FCAS
     C                   PARM                    @@TGRP
     C                   PARM                    @@TEMP
     C                   PARM                    @@TDIV
     C                   PARM                    @@TFYR
     C                   PARM                    @@TCAS
     C                   PARM      TRNXFL        @@TRNX            1
     C*
     C*  Update the claim total fields in the employer detail file.
     C*
     C                   EXSR      CLMTOT
     C*
     C*  call API to update all WCMC claim files
     C*
     C*** Begin Add *** 12/26/24 *******************************************
     C**********         CALL      'FA800R01'
     C                   CALL(e)   'FA800R01'
     C*** End   Add *** 12/26/24 *******************************************
     C                   PARM                    @@CO#
     C                   PARM                    @@FGRP
     C                   PARM                    @@FEMP
     C                   PARM                    @@FDIV
     C                   PARM                    @@FFYR
     C                   PARM                    @@FCAS
     C                   PARM                    @@TGRP
     C                   PARM                    @@TEMP
     C                   PARM                    @@TDIV
     C                   PARM                    @@TFYR
     C                   PARM                    @@TCAS
     C*
     C* call API to transfer DARTS records
     C*
     C*** Begin add *** 01/03/08 **************************************
     C     SCFYR         IFNE      XXTFYR
     C     SCCASE        ORNE      XXTCAS
     C*** End add ***** 01/03/08 **************************************
     C*** Begin add *** 01/03/08 **************************************
     C     SCFFND        ORNE      SCTFND
     C*** End   add *** 01/03/08 **************************************
     C*
     C                   CLEAR                   DFKY
     C                   CLEAR                   DTKY
     C     @@CO#         CAT       @@FGRP:0      DFKY
     C                   CAT       @@FFYR:0      DFKY
     C                   CAT       @@FCAS:0      DFKY
     C     @@CO#         CAT       @@TGRP:0      DTKY
     C                   CAT       @@TFYR:0      DTKY
     C                   CAT       @@TCAS:0      DTKY
     C                   MOVE      *BLANK        DRTCD
     C*
     C                   CALL      'SC900C'
     C                   PARM                    DFKY
     C                   PARM                    DTKY
     C                   PARM                    DQUE
     C                   PARM                    DMODE
     C                   PARM                    DRTCD
     C*
     C*** Begin add *** 01/03/08 **************************************
     C                   ENDIF
     C*** End add ***** 01/03/08 **************************************
     C*
     C                   ENDSR
     C*** END ADD ***** 02/15/00 **************************************
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      *ZEROS        SCFFND
     C                   MOVE      *ZEROS        SCEMP#
     C                   MOVE      *ZEROS        SCTFND
     C*** BEGIN ADD *** 02/15/00 **************************************
     C                   MOVE      *ZEROS        SCFYR
     C                   MOVE      *ZEROS        SCCASE
     C                   MOVE      *ZEROS        SCTEMP
     C                   MOVE      *ZEROS        SCTDIV
     C*** END ADD ***** 02/15/00 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C*  Get the 'From' and 'To' bank account numbers.
     C*
     C*        'From' bank account
     C*
     C                   Z-ADD     SCFFND        XXFND
     C     FNKY          CHAIN     PF201                              60
     C  N60              MOVE      F2CTYP        QQCALL
     C*
     C                   MOVE      ##CO#         QQCO#
     C                   MOVE      SCFFND        QQFND
     C                   MOVE      DSDATE        QQSRDT
     C                   CALL      'WA023R'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM      'WC'          QQAPPL
     C                   PARM                    QQSRDT
     C                   PARM                    QQACCT                         Bank Acct #
     C                   PARM                    QQCALL                         (V)oucher/(C)heck
     C                   PARM      *BLANK        QQHDCY
     C                   PARM      *BLANK        QQCHKQ
     C                   PARM      *BLANK        QQFTYP
     C                   PARM      *BLANK        QQRTCD
     C                   PARM      *ZERO         QQEFDT
      *
     C     QQRTCD        IFEQ      *BLANKS
     C                   MOVE      QQACCT        ETOACT
     C                   ELSE
     C                   MOVE      *BLANKS       ETOACT
     C                   ENDIF
     C*
     C*        'To' bank account
     C*
     C                   Z-ADD     SCTFND        XXFND
     C     FNKY          CHAIN     PF201                              60
     C  N60              MOVE      F2CTYP        QQCALL
     C*
     C                   MOVE      SCTFND        QQFND
     C                   MOVE      DSDATE        QQSRDT
     C                   CALL      'WA023R'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM      'WC'          QQAPPL
     C                   PARM                    QQSRDT
     C                   PARM                    QQACCT                         Bank Acct #
     C                   PARM                    QQCALL                         (V)oucher/(C)heck
     C                   PARM      *BLANK        QQHDCY
     C                   PARM      *BLANK        QQCHKQ
     C                   PARM      *BLANK        QQFTYP
     C                   PARM      *BLANK        QQRTCD
     C                   PARM      *ZERO         QQEFDT
      *
     C     QQRTCD        IFEQ      *BLANKS
     C                   MOVE      QQACCT        ETACCT
     C                   ELSE
     C                   MOVE      *BLANKS       ETACCT
     C                   ENDIF
     C*
     C* Obtain the next employer number from the 'To' Group Master File
     C*
     C                   Z-ADD     1             FNNAEN
     C                   Z-ADD     SCTFND        XXFND
     C     FNKY          CHAIN     PFN01                              99
     C*** Begin add *** 12/05/01 **************************************
     C     FNAUTE        IFEQ      'Y'
     C*** End add ***** 12/05/01 **************************************
     C*
     C                   ADD       1             FNNAEN
      *** Beg Add ***** 05/23/24 ***********************
     C                   MOVE      Q1USER        FNUSER
     C                   MOVE      Q1PGM         FNPGM
     C                   Z-ADD     SAVEDT        FNDT
     C                   MOVE      DSTIME        FNTIME
      *** End Add ***** 05/23/24 ***********************
     C  N99              UPDATE    PFN01
     C  N99              FEOD      WMFNL
     C     FNNAEN        SUB       1             XXTEMP
     C*
     C*** Begin add *** 12/05/01 **************************************
     C                   ELSE
     C*
     C     FNKY          SETGT     PEM02
     C     FNKY          READPE    PEM02                                  99
     C  N99EMEMP#        ADD       1             XXTEMP
     C   99              Z-ADD     1             XXTEMP
     C*
     C                   ENDIF
     C*** End add ***** 12/05/01 **************************************
     C*
     C                   MOVE      SCFFND        @@FGRP
     C                   MOVE      SCEMP#        @@FEMP
     C                   MOVE      SCTFND        @@TGRP
     C                   MOVE      XXTEMP        @@TEMP
     C*
     C* Obtain Divisions of the 'From' Employer to move
     C*
     C                   MOVE      SCFFND        XXFND
     C                   MOVE      SCEMP#        XXEMP#
     C     EMKY          CHAIN     PEM02                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   MOVE      EMDIV         @@FDIV
     C                   MOVE      EMDIV         @@TDIV
     C*
     C* Write record to WCMS Employer Transfer File
     C*
     C                   MOVE      ##CO#         ETCO#
     C                   MOVE      SCTFND        ETFND
     C                   MOVE      @@TEMP        ETEMP#
     C                   MOVE      @@TDIV        ETDIV
     C                   MOVE      *ZEROS        ETFYR
     C                   MOVE      *ZEROS        ETCASE
     C                   MOVE      SCFFND        ETOFND
     C                   MOVE      SCEMP#        ETOEMP
     C                   MOVE      EMDIV         ETODIV
     C                   MOVE      *ZEROS        ETOFYR
     C                   MOVE      *ZEROS        ETOCAS
     C*
     C                   MOVE      Q1USER        ETUSER
     C                   MOVE      Q1PGM         ETPGM
     C                   MOVE      SAVEDT        ETDT
     C                   MOVE      DSTIME        ETTIME
     C                   MOVE      Q1USER        ETIUSR
     C                   MOVE      Q1PGM         ETIPGM
     C                   MOVE      SAVEDT        ETIDT
     C                   MOVE      DSTIME        ETITIM
     C*
     C                   WRITE     PET01
     C*
     C*  call API to update all WCMS employer files
     C*
     C                   CALL      'WA036R'
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FGRP            3
     C                   PARM                    @@FEMP            9
     C                   PARM                    @@FDIV            5
     C                   PARM                    @@TGRP            3
     C                   PARM                    @@TEMP            9
     C                   PARM                    @@TDIV            5
     C*
     C*  call API to update all WCMC employer files
     C*
     C*** Begin Add *** 12/26/24 *******************************************
     C**********         CALL      'FA800R'
     C                   CALL(e)   'FA800R'
     C*** End   Add *** 12/26/24 *******************************************
     C                   PARM                    @@CO#
     C                   PARM                    @@FGRP
     C                   PARM                    @@FEMP
     C                   PARM                    @@FDIV
     C                   PARM                    @@TGRP
     C                   PARM                    @@TEMP
     C                   PARM                    @@TDIV
     C*
     C* Call API to transfer Employer DARTS records
     C*
     C                   CLEAR                   DFKY
     C                   CLEAR                   DTKY
     C     @@CO#         CAT       @@FGRP:0      DFKY
     C                   CAT       @@FEMP:0      DFKY
     C                   CAT       @@FDIV:0      DFKY
     C     @@CO#         CAT       @@TGRP:0      DTKY
     C                   CAT       @@TEMP:0      DTKY
     C                   CAT       @@TDIV:0      DTKY
     C*
     C                   CALL      'SC900C'
     C                   PARM                    DFKY             40
     C                   PARM                    DTKY             40
     C                   PARM                    DQUE             10
     C                   PARM                    DMODE             1
     C                   PARM                    DRTCD             1
     C*
     C* Transfer all claims for this employer.
     C*
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C* obtain next available claim number from the 'To' Group
     C* Master File
     C*
     C                   Z-ADD     1             FNNACS
     C                   Z-ADD     SCTFND        XXFND
     C     FNKY          CHAIN     PFN01                              99
     C                   ADD       1             FNNACS
      *** Beg Add ***** 05/23/24 ***********************
     C                   MOVE      Q1USER        FNUSER
     C                   MOVE      Q1PGM         FNPGM
     C                   Z-ADD     SAVEDT        FNDT
     C                   MOVE      DSTIME        FNTIME
      *** End Add ***** 05/23/24 ***********************
     C  N99              UPDATE    PFN01
     C  N99              FEOD      WMFNL
     C     FNNACS        SUB       1             XXTCAS
     C*
     C* Write record to WCMS Employer Transfer File
     C*
     C                   MOVE      ##CO#         ETCO#
     C                   MOVE      SCTFND        ETFND
     C                   MOVE      @@TEMP        ETEMP#
     C                   MOVE      EMDIV         ETDIV
     C                   MOVE      CMFYR         ETFYR
     C                   MOVE      XXTCAS        ETCASE
     C                   MOVE      SCFFND        ETOFND
     C                   MOVE      SCEMP#        ETOEMP
     C                   MOVE      EMDIV         ETODIV
     C                   MOVE      CMFYR         ETOFYR
     C                   MOVE      CMCASE        ETOCAS
     C*
     C                   MOVE      Q1USER        ETUSER
     C                   MOVE      Q1PGM         ETPGM
     C                   MOVE      SAVEDT        ETDT
     C                   MOVE      DSTIME        ETTIME
     C                   MOVE      Q1USER        ETIUSR
     C                   MOVE      Q1PGM         ETIPGM
     C                   MOVE      SAVEDT        ETIDT
     C                   MOVE      DSTIME        ETITIM
     C*
     C                   WRITE     PET01
      *
      *  call API to update all WCMS claim files
      *
     C                   MOVE      CMCASE        @@FCAS
     C                   MOVE      CMFYR         @@FFYR
     C                   MOVE      CMFYR         @@TFYR
     C                   MOVE      XXTCAS        @@TCAS
     C                   CALL      'WA037R'
     C                   PARM                    @@CO#
     C                   PARM                    @@FGRP
     C                   PARM                    @@FEMP
     C                   PARM                    @@FDIV
     C                   PARM                    @@FFYR            3
     C                   PARM                    @@FCAS            7
     C                   PARM                    @@TGRP
     C                   PARM                    @@TEMP
     C                   PARM                    @@TDIV
     C                   PARM                    @@TFYR            3
     C                   PARM                    @@TCAS            7
     C*** BEGIN ADD *** 02/15/00 **************************************
     C                   PARM      TRNXFL        @@TRNX            1
     C*** END ADD ***** 02/15/00 **************************************
     C*
     C*  call API to update all WCMC claim files
     C*
     C*** Begin Add *** 12/26/24 *******************************************
     C**********         CALL      'FA800R01'
     C                   CALL(e)   'FA800R01'
     C*** End   Add *** 12/26/24 *******************************************
     C                   PARM                    @@CO#
     C                   PARM                    @@FGRP
     C                   PARM                    @@FEMP
     C                   PARM                    @@FDIV
     C                   PARM                    @@FFYR
     C                   PARM                    @@FCAS
     C                   PARM                    @@TGRP
     C                   PARM                    @@TEMP
     C                   PARM                    @@TDIV
     C                   PARM                    @@TFYR
     C                   PARM                    @@TCAS
     C*
     C* call API to transfer DARTS records
     C*
     C                   CLEAR                   DFKY
     C                   CLEAR                   DTKY
     C     @@CO#         CAT       @@FGRP:0      DFKY
     C                   CAT       @@FFYR:0      DFKY
     C                   CAT       @@FCAS:0      DFKY
     C     @@CO#         CAT       @@TGRP:0      DTKY
     C                   CAT       @@TFYR:0      DTKY
     C                   CAT       @@TCAS:0      DTKY
     C                   MOVE      *BLANK        DRTCD
     C*
     C                   CALL      'SC900C'
     C                   PARM                    DFKY
     C                   PARM                    DTKY
     C                   PARM                    DQUE             10
     C                   PARM                    DMODE             1
     C                   PARM                    DRTCD             1
     C*
     C*** BEG ADD *** 01/09/04 ****************************
     C*
     C*  Reset key from fund chain
     C*
     C                   MOVE      SCFFND        XXFND
     C*** END ADD *** 01/09/04 ****************************
     C*
     C*  Retrieve next claim for this employer.
     C*
     C     CMKY          READE     PCM01                                  99
     C                   ENDDO
     C*
     C*  Retrieve the next division for employer being transferred.
     C*
     C     EMKY          READE     PEM02                                  99
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         IFEQ      *ON
     C                   EXSR      KJDO
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   MOVE      ##CO#         @@CO#
     C*
     C     Q1USER        CHAIN     PUP01                              99
     C*
     C*** Begin add *** 01/03/08 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*** End add ***** 01/03/08 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
      *** BEGIN ADD *** 02/15/00 **************************************
     C*===============================================================
     C* RVDED - This subroutine calls W8024R which determines if the old
     C*   employer was on a deductible plan and, if so, reverses the deduct
     C*   transactions.
     C*===============================================================
     C     RVDED         BEGSR
     C*
     C                   CLEAR                   XXAMTB
     C*
     C                   MOVE      SCFFND        XXFND
     C                   MOVE      SCFYR         XXFYR
     C                   MOVE      SCCASE        XXCASE
     C     DDKY          CHAIN     PDD01                              99
     C     *IN99         DOWEQ     *OFF
     C                   ADD       DDAMTB        XXAMTB
     C     DDKY          READE     PDD01                                  99
     C                   ENDDO                                                  PDD01 RECORD
     C*
     C                   MOVE      SCFFND        QQFND
     C                   MOVE      SCFYR         QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   MOVE      CMEMP#        QQFEMP
     C                   MOVE      CMDIV         QQFDIV
     C                   MOVE      *ZEROS        QQTEMP
     C                   MOVE      *ZEROS        QQTDIV
     C                   MOVE      XXAMTB        QQAMTB
     C                   CALL      'W8024R'
     C                   PARM      *BLANK        QQCALL            1
     C                   PARM                    @@CO#
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQFEMP            9
     C                   PARM                    QQFDIV            5
     C                   PARM                    QQTEMP            9
     C                   PARM                    QQTDIV            5
     C                   PARM                    QQAMTB            9
     C                   PARM      *BLANK        QQRTCD            1
     C*
     C                   ENDSR
     C*** END ADD ***** 02/15/00 **************************************
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C*** BEGIN DLT *** 02/15/00 **************************************
     C******************** EXSR MVSCDB
     C*** END DLT ***** 02/15/00 **************************************
     C*** BEGIN CHG *** 02/15/00 **************************************
     C     TRNXFL        IFEQ      'E'
     C                   EXSR      MVSCDB
     C                   ELSE
     C     TRNXFL        IFEQ      'C'
     C                   EXSR      MVCLM
     C                   ENDIF
     C                   ENDIF
     C*** END ADD ***** 02/15/00 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C*** BEGIN DLT *** 02/15/00 **************************************
     C******************** EXSR MVSCDB
     C*** END DLT ***** 02/15/00 **************************************
     C*** BEGIN CHG *** 02/15/00 **************************************
     C     TRNXFL        IFEQ      'E'
     C                   EXSR      MVSCDB
     C                   ELSE
     C     TRNXFL        IFEQ      'C'
     C                   EXSR      MVCLM
     C                   ENDIF
     C                   ENDIF
     C*** END ADD ***** 02/15/00 **************************************
     C*
     C                   ENDSR
