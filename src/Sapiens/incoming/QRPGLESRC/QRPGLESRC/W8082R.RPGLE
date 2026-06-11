     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H/TITLE W8082R - WORK WITH RECURRING COMP INFO
     H Bnddir('WTBNDDIR')
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL MAINTAIN RECURRING COMP INFO *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0070R                                                     *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    W8082R02                                                   *
     H*                                                               *
     H*  CREATED: 05/11/01  PROGRAMMER: C:53  PROJECT: 114WBY         *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*
     H* 08/18/06 C:53  Project: 114WMF                                *
     H*                Back to Base.                                  *
     H*                                                               *
     H* 09/19/05 C:132 Project: WC114 - 49065                         *
     H*                Added code to load the correct group name in   *
     H*                corresponding screen field group name.         *
     H*                                                               *
     H* 08/06/01 C:15  Project: 114WBY                                *
     H*                Added code to load the payee name from the     *
     H*                appropriate file (based on whether the record  *
     H*                is for the claimant, a dependent, or a vendor) *
     H*                because the name and address are no longer     *
     H*                populated in WMCRP.                            *
     H*****************************************************************
     H*
     FW8082F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWMCRP     IF   E             DISK
     F                                     RENAME(PCR01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     F*** Begin chg *** 08/06/01 **************************************
     F*******WMCRL2  IF  E           K        DISK
     FWMCRL     IF   E           K DISK
     F*** End chg ***** 08/06/01 **************************************
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCR01:DBRCDF)
     FWMCMP     IF   E             DISK
     F*** Begin add *** 08/06/01 **************************************
     FWMPDL     IF   E           K DISK
     FWDACL     IF   E           K DISK
     F*** End add ***** 08/06/01 **************************************
     F*
      ***** BEG ADD ***** 09/19/05 ************************************
     FWMFNL     IF   E           K DISK
      ***** END ADD ***** 09/19/05 ************************************
     F*
      /COPY SCOPYWW01D
      ***  Begin Add  ***  06/09/20  ****************************
      /copy sPrMask
      /copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(10) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(10) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D                 DS                  INZ
     D  SCDATA                 1     74
     D  XSPNAM                 1     20
     D  XSBGDT                23     33
     D  XSENDT                35     45
     D  XSLPDT                47     57
     D  XSBR2                 59     60
     D  XSAMT                 63     74
     D                 DS                  INZ
     D  SCYRCS                 1     11
     D  XXFYR                  1      3  0
     D  XXDASH                 4      4
     D  XXCASE                 5     11  0
     D                 DS                  INZ
     D  XXSS#                  1      9
     D  XXSS1                  1      3
     D  XXSS2                  4      5
     D  XXSS3                  6      9
      *****  Begin Del  ***  06/09/20  ***********************
     D*                DS                  INZ
     D* SCSS#                  1     11
     D* X2SS1                  1      3
     D* X2DSH1                 4      4
     D* X2SS2                  5      6
     D* X2DSH2                 7      7
     D* X2SS3                  8     11
      *****  End Del  ***  06/09/20  ***********************
     D*
     D                 DS
     D  LN1                    1     79
     D  LN1A                   1      6
     D  LN1B                   8     10
     D  LN1C                  12     71
     D*
     D                 DS
     D  LN2                    1     79
     D  SCCLNM                39     58
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Recurring Comp Info')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W8082R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(8)
     D CHGNUM          C                   CONST('SA020R')
     D CLMAPI          C                   CONST('WA006R')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@RRN         ##RRN2            9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     CHGPL         PLIST
     C                   PARM                    ##NUM            80
     C                   PARM                    ##MASK           10
     C*
     C     CLMPL         PLIST
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    LN1
     C                   PARM                    LN2
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     KEY           KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*** Begin dlt *** 08/06/01 **************************************
     C**********           KFLD           KKPNAM
     C**********           KFLD           KKBGDT
     C*** End dlt ***** 08/06/01 **************************************
     C                   KFLD                    KKSEQ#
     C*
     C*** Begin add *** 08/06/01 **************************************
     C*  Key list for the provider file
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C*  Key list for the provider file
     C*
     C     ACKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKASQ#
     C*** End add ***** 08/06/01 **************************************
      *
      ***** BEG ADD ***** 09/19/05 ************************************
     C*
     C*  Key list for the group master file
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
      ***** END ADD ***** 09/19/05 ************************************
      *
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CRCO#         KKCO#
     C     *LIKE         DEFINE    CRFND         KKFND
     C     *LIKE         DEFINE    CRFYR         KKFYR
     C     *LIKE         DEFINE    CRCASE        KKCASE
     C*** Begin dlt *** 08/06/01 **************************************
     C********** *LIKE     DEFN CRPNAM    KKPNAM
     C********** *LIKE     DEFN CRBGDT    KKBGDT
     C*** End dlt ***** 08/06/01 **************************************
     C     *LIKE         DEFINE    CRSEQ#        KKSEQ#
     C*** Begin add *** 08/06/01 **************************************
     C     *LIKE         DEFINE    CRFEIN        KKFEIN
     C     *LIKE         DEFINE    CRMOD         KKMOD
     C     *LIKE         DEFINE    CRASQ#        KKASQ#
     C*** End add ***** 08/06/01 **************************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C                   MOVE      *ON           *IN11
     C                   ENDSR
     C*================================================================
     C* CVTDBF - Convert database fields to screen format
     C*================================================================
     C     CVTDBF        BEGSR
     C                   Z-ADD     CRBGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         ##NUM            80
     C                   MOVEL(P)  'DATE'        ##MASK           10
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  ##NUM         XSBGDT
     C*
     C                   Z-ADD     CRENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         ##NUM            80
     C                   MOVEL(P)  'DATE'        ##MASK           10
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  ##NUM         XSENDT
     C*
     C                   Z-ADD     CRLPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         ##NUM            80
     C                   MOVEL(P)  'DATE'        ##MASK           10
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  ##NUM         XSLPDT
     C*
     C                   MOVEL(P)  CRAMT         ##NUM
     C                   MOVEL     'X,XXX.XX'    ##MASK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  ##NUM         XSAMT
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C     @@RRN         CHAIN     PCM01                              99
     C                   MOVE      CMCO#         KKCO#
     C                   MOVE      CMFND         KKFND
     C                   MOVE      CMFYR         KKFYR
     C                   MOVE      CMCASE        KKCASE
     C*** Begin dlt *** 08/06/01 **************************************
     C**********           MOVE *LOVAL    KKPNAM
     C**********           MOVE *LOVAL    KKBGDT
     C*** End dlt ***** 08/06/01 **************************************
     C                   MOVE      *LOVAL        KKSEQ#
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C*** Begin dlt *** 08/06/01 **************************************
     C********** *IN21     IFEQ *ON
     C********** SCPST1    ANDNE*BLANKS
     C**********           MOVELSCPST1    KKPNAM
     C**********           ENDIF
     C*** End dlt ***** 08/06/01 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   Z-ADD     SCCO#         KKCO#
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCFYR         KKFYR
     C                   Z-ADD     SCCASE        KKCASE
     C*** Begin dlt *** 08/06/01 **************************************
     C**********           MOVELSCPNAM    KKPNAM    P
     C**********           Z-ADDSCBGDT    KKBGDT
     C*** End dlt ***** 08/06/01 **************************************
     C                   Z-ADD     SCSEQ#        KKSEQ#
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   EXSR      CVTDBF
     C*
     C*** Begin dlt *** 08/06/01 **************************************
     C***********          MOVELCRPNAM    XSPNAM
     C*** End dlt ***** 08/06/01 **************************************
     C                   MOVE      CRBR2         XSBR2
     C*
     C                   Z-ADD     CRCO#         SCCO#
     C                   Z-ADD     CRFND         SCFND
     C                   Z-ADD     CRFYR         SCFYR
     C                   Z-ADD     CRCASE        SCCASE
     C*** Begin dlt *** 08/06/01 **************************************
     C***********          MOVELCRPNAM    SCPNAM    P
     C*** End dlt ***** 08/06/01 **************************************
     C                   Z-ADD     CRBGDT        SCBGDT
     C                   Z-ADD     CRSEQ#        SCSEQ#
     C*** Begin add *** 08/06/01 **************************************
     C*  Load the name and address based on whether it is the claimant,
     C*  a provider, or a dependent.
     C*
     C                   MOVE      *BLANKS       SCPNAM
     C     CRSS#         IFNE      *BLANKS
     C     CRFEIN        ORNE      *ZEROS
     C*-----
     C     CRSS#         IFNE      *BLANKS                                      load claimant
     C                   MOVEL(P)  CMFSN         SCPNAM
     C                   CAT       CMMID:1       SCPNAM
     C                   CAT       CMLSN:1       SCPNAM
     C                   ENDIF
     C*-----
     C     CRFEIN        IFNE      *ZEROS                                       load provider
     C*
     C                   MOVE      CRFEIN        KKFEIN
     C                   MOVE      CRMOD         KKMOD
     C     PDKY          CHAIN     PPD01                              99
     C     PDDBA1        IFEQ      'Y'
     C  N99              MOVEL(P)  PDADR1        SCPNAM
     C                   ELSE
     C  N99              MOVEL(P)  PDNAME        SCPNAM
     C                   ENDIF
     C*
     C                   ENDIF
     C*-----
     C                   ELSE
     C*                                                    load dependent
     C                   MOVE      CRASQ#        KKASQ#
     C     ACKY          CHAIN     PAC01                              99
     C  N99              MOVEL     ACPNAM        SCPNAM
     C*
     C                   ENDIF
     C*
     C                   MOVEL     SCPNAM        XSPNAM
     C*** End add ***** 08/06/01 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
      *****  Beg Del  ***  06/09/20  ***********************
     C*****              MOVE      '-'           XXDASH
     C*****              MOVE      '-'           X2DSH1
     C*****              MOVE      '-'           X2DSH2
      *****  End Del  ***  06/09/20  ***********************
     C*
     C* Get header info from the claim file
     C*
     C                   MOVE      CMFND         SCFND
     C                   MOVE      CMFYR         XXFYR
     C                   MOVE      CMCASE        XXCASE
     C                   MOVE      CMJRST        SCJRST
     C                   MOVE      CMNCCI        SCNCCI
     C                   MOVE      CMLTFL        SCTYPE
     C* Get fund name
      *
      ***** BEG CHG ***** 09/19/05 ************************************
     C                   MOVE      ##FNAM        SCFNAM
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL(P)  FNNAME        SCFNAM
      ***** END CHG ***** 09/19/05 ************************************
     C* Format name
     C**                   MOVE CMLSN     SCCLNM
     C**                   MOVE CMFSN     SCCLNM
     C* Format SS#
      *****  Begin Chg  ***  06/09/20  **********************
     C*****              MOVE      CMSS#         XXSS#
     C*****              MOVE      XXSS1         X2SS1
     C*****              MOVE      XXSS2         X2SS2
     C*****              MOVE      XXSS3         X2SS3
      /Free
       scSS# = DisplaySS#(MaskData('SSN':'CMSS#':CMSS#));
      /End-free
      ******  End Chg  ***  06/09/20  **********************
     C* Format DOI
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         ##NUM            80
     C                   MOVEL(P)  'DATE'        ##MASK           10
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  ##NUM         SCACDT
     C*
     C                   CALL      CLMAPI        CLMPL
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEY           READE     DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY           READPE    DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C                   SELECT
     C     CRCO#         WHENNE    KKCO#
     C                   MOVE      *OFF          MEETSS
     C     CRFND         WHENNE    KKFND
     C                   MOVE      *OFF          MEETSS
     C     CRFYR         WHENNE    KKFYR
     C                   MOVE      *OFF          MEETSS
     C     CRCASE        WHENNE    KKCASE
     C                   MOVE      *OFF          MEETSS
     C                   ENDSL
     C*
     C                   ENDSR
** Column headings
Opt Payee Name            Begin date  End date    Last pmt    PV1       Amount
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
