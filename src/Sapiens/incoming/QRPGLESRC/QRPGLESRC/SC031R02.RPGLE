     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*===============================================================*
     H*===============================================================*
     H/TITLE SC030R02 - DARTS SECURITY MAINTENANCE
     H*===============================================================*
     H* DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW  *
     H*                OR ADD RECORDS IN THE DARTS SECURITY FILE.     *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   S0005R - WORK WITH USER PARAMETERS                          *
     H*   S0005R02 - USER PARAMETERS UPDATE/DISPLAY                   *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*   NONE                                                        *
     H*                                                               *
     H* CREATED   : 11/30/99                                          *
     H* PROGRAMMER: C:113                                             *
     H* PROJECT   : DR079                                             *
     H*===============================================================*
     H* REVISIONS:                                                    *
     H*                                                               *
     h*  04/08/16 C:33  Project: Elevate                              *
     h*                 Bind SYSM modules by copy                     *
     H*                                                               *
     h*  07/02/15 C:306 Support: SEQ86962 / 31805                     *
     h*                 Added the email validation check              *
     h*
     H* 10/16/12  C:33  Project: SY266
     H*                 Always default SLvDaD to 'D' as V13 forward
     H*                  will require iDARTS
     H*
     H* 06/20/01  C:117 SUPPORT: DR109  SEQ# 27718
     H*                 CHANGED CONDITION IN ERROR CHECKING FOR
     H*                 FLAG TO ALLOW WRITE OF E-MAIL IN DARTS
     H*              ***PUT ORIGIONAL CONDITION BACK; CHG'D ERR MSG
     H*
     H* 08/09/2000 C:128 PROJECT: SY041                               *
     H*            Changed logical for one that includes all status's *
     H*            Added the ability to maintain the status field.    *
     H*                                                               *
     H*                                                               *
     H* 01/17/2000 C:113 PROJECT: DR086                               *
     H*            Add flags for writing an email DARTS remark.       *
     H*                                                               *
     H* ------------------------------------------------------------- *
     H*  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
     H* ------------------------------------------------------------- *
     H*===============================================================*
     H* INDICATOR USAGE                                               *
     H* SCR01:                                                        *
     H*  10-33 EDIT FUNCTION KEYS FLAGS MUST BE Y OR N                *
     H*  45    SECURITY LEVEL REQUIRED                                *
     H*  46    CHANGE REMARK FLAG MUST BE Y OR N                      *
     H*  47    DELETE REMARK FLAG MUST BE Y OR N                      *
     H*  48    VALID CUSTOM ID REQUIRED                               *
     H*  49    CHANGE CUSTOM ID FLAG MUST BE Y OR N                   *
     H*  51    USER DESCRIPTION REQUIRED                              *
     H*  52    EDIT OFFICE DOCUMENT FLAG MUST BE Y OR N               *
     H*  53    DISPLAY OFFICE DOCUMENT FLAG MUST BE Y OR N            *
     H*  54    ADD OFFICE DOCUMENT FLAG MUST BE Y OR N                *
     H*  55    DELETE OF COMMENT FLAG MUST BE Y OR N                  *
     H*  56    SCM0521-FOLDER DOES NOT EXIST                          *
     H*  57    SCM0517-NOT AUTHORIZED TO FOLDER                       *
     H*  61    SCVDTA MUST BE Y OR BLANK                              *
     H*  62    SCVDTD MUST BE Y OR BLANK                              *
     H*  63    SCVDTD CANNOT BE Y IF SCVDTA IS BLANK                  *
     H*  65    IF ENTERED, VALID REMARK TYPE CODE REQUIRED            *
     H*  67    SCALWR MUST BE Y OR BLANK                              *
     H*===============================================================*
     FSC031F02  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     F*  Physical file definition (for maintaining record)
     FSMSLP     UF A E             DISK
     F                                     RENAME(PSL01:DBRCDF)
     FSMSLL2    UF   E           K DISK
     F                                     INFDS(DBFDS)
     FSMCGL     IF   E           K DISK
     FSTCTL1    IF   E           K DISK
     FSSYLF     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D                 DS
     D  SCF                    1     24
     D                                     DIM(24)                              FUNCTION FLAGS
     D  SCFL01                 1      1
     D  SCFL02                 2      2
     D  SCFL03                 3      3
     D  SCFL04                 4      4
     D  SCFL05                 5      5
     D  SCFL06                 6      6
     D  SCFL07                 7      7
     D  SCFL08                 8      8
     D  SCFL09                 9      9
     D  SCFL10                10     10
     D  SCFL11                11     11
     D  SCFL12                12     12
     D  SCFL13                13     13
     D  SCFL14                14     14
     D  SCFL15                15     15
     D  SCFL16                16     16
     D  SCFL17                17     17
     D  SCFL18                18     18
     D  SCFL19                19     19
     D  SCFL20                20     20
     D  SCFL21                21     21
     D  SCFL22                22     22
     D  SCFL23                23     23
     D  SCFL24                24     24
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('DARTS Security File')
     D COMAPI          C                   CONST('SA005R')
     D #OFSCR          C                   CONST(2)
     D/copy sPrGenErr
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    QQUSER           10
     C                   PARM                    QQSEC             3
     C                   PARM                    QQDESC           40
     C                   PARM                    QQCALL            1
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*   KEY LIST FOR CUSTOM GROUP ID MASTER FILE
     C*
     C     CGKEY         KLIST
     C                   KFLD                    XXCSID            2
     C*
     C*   KEY LIST FOR THE SECURITY FILE
     C*
     C     SLKEY         KLIST
     C                   KFLD                    XXUSR            10
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C/COPY SCOPYMT01C
     C*
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
     C** Description cannot be blank.
     C  N90SCUDSC        IFEQ      *BLANKS
     C                   SETON                                        51  90
     C                   ENDIF
     C*
     C** Security Level cannot equal zero.
     C  N90SCSECL        IFEQ      *ZEROS
     C                   SETON                                        45  90
     C                   ENDIF
     C*
     C** All function flags must be Y or N.
     C** Uses error indicators 20-43
     C  N90              DO        24            A                 5 0
     C     A             ADD       9             ER                2 0
     C     SCF(A)        IFNE      'Y'
     C     SCF(A)        ANDNE     'N'
     C                   MOVE      *ON           *IN(ER)
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCF,A = Y/N
     C                   ENDDO                                                  DO 24 A
     C*
     C** Restrict all DARTS access
     C  N90SCRACC        IFNE      'Y'
     C     SCRACC        ANDNE     *BLANK
     C                   SETON                                        58  90
     C                   ENDIF                                                  SCRACC != Y/
     C*
     C** Visual DARTS access and default fields
     C  N90SCVDTA        IFNE      'Y'
     C     SCVDTA        ANDNE     *BLANK
     C                   SETON                                        61  90
     C                   ENDIF                                                  SCVDTA != Y/*BLANK
     C*
     C     SCVDTD        IFNE      'Y'
     C     SCVDTD        ANDNE     *BLANK
     C                   SETON                                        62  90
     C                   ENDIF                                                  SCVDTD != Y/*BLANK
     C*
     C  N90SCVDTD        IFEQ      'Y'
     C     SCVDTA        ANDEQ     *BLANK
     C                   SETON                                        63  90
     C                   ENDIF                                                  SCVDTD=Y/SCVDTA=' '
     C*
     C  N90              SETON                                        66
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C** Description cannot be blank.
     C  N90SCUDSC        IFEQ      *BLANKS
     C                   SETON                                        51  90
     C                   ENDIF
     C*
     C** Security Level cannot equal zero.
     C  N90SCSECL        IFEQ      *ZEROS
     C                   SETON                                        45  90
     C                   ENDIF
     C*
     C** If present, Remarks Type Code must be valid.
     C  N90SCRMCD        IFNE      *BLANKS
     C     SCRMCD        CHAIN     PCT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        65  90
     C                   ENDIF                                                  *IN60 = *ON
     C                   ENDIF                                                  SCRMCD != *BLANKS
     C*
     C** Change Remark Type flag must be Y or N.
     C  N90SCCCTF        IFNE      'Y'
     C     SCCCTF        ANDNE     'N'
     C                   SETON                                        46  90
     C                   ENDIF                                                  SCCCTF! = Y/N
     C*
     C** Delete of Comment flag must be Y or N.
     C  N90SCDELC        IFNE      'Y'
     C     SCDELC        ANDNE     'N'
     C                   SETON                                        47  90
     C                   ENDIF
     C*
     C** Validate the Custom Type.
     C  N90SCRACC        IFNE      'Y'
     C                   MOVE      *OFF          *IN59
     C                   MOVE      *BLANKS       XXCSID
     C                   MOVEL     SCCSID        XXCSID
     C     CGKEY         CHAIN     PCG01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        48  90
     C                   ENDIF                                                  *IN60 = *ON
     C*
     C                   ELSE
     C                   MOVE      *ON           *IN59
     C                   ENDIF                                                  SCRACC != Y/N
     C*
     C** Allow Change of Custom ID flag must be 'Y' OR 'N'
     C  N90SCCCIF        IFNE      'Y'
     C     SCCCIF        ANDNE     'N'
     C                   SETON                                        49  90
     C                   ENDIF                                                  SCCCIF != Y/N
     C*
     C** Allow Display Office Document flag must be Y or N.
     C  N90SCDDC         IFNE      'Y'
     C     SCDDC         ANDNE     'N'
     C                   SETON                                        53  90
     C                   ENDIF                                                  SCDDC != Y/N
     C*
     C** Allow Edit Office Document flag must be Y or N.
     C  N90SCEDC         IFNE      'Y'
     C     SCEDC         ANDNE     'N'
     C                   SETON                                        52  90
     C                   ENDIF                                                  SCEDC != Y/N
     C*
     C** Allow Add Office Document flag must be Y or N.
     C  N90SCADC         IFNE      'Y'
     C     SCADC         ANDNE     'N'
     C                   SETON                                        54  90
     C                   ENDIF                                                  SCADC != Y/N
     C*
     C** If entered, validate folder.
     C** Only perform this function if OV/400 is installed.
     C  N90SCFOLD        IFNE      *BLANKS
     C     XXOFIF        ANDEQ     'Y'
     C**   CHANGE TO INCLUDE THE FOLDER PREFIX IF NECESSARY
     C                   CALL      'SA030R'
     C                   PARM      *BLANKS       ##SUFX            3
     C                   PARM      *BLANKS       ##PFLR           80
     C*
     C     ##PFLR        IFNE      *BLANKS
     C     ##PFLR        CAT(P)    '/':0         @@WK3            80
     C                   CAT(P)    SCFOLD:0      @@WK3
     C                   ELSE
     C                   MOVEL     *BLANKS       @@WK3
     C                   MOVEL     SCFOLD        @@WK3
     C                   ENDIF
     C*
     C                   MOVE      ' '           QQRTCD            1
     C*
     C                   CALL      'SVALFLR'
     C                   PARM                    @@WK3
     C                   PARM                    QQRTCD
     C*
     C                   SELECT
     C     QQRTCD        WHENEQ    '1'
     C                   SETON                                        56  90
     C*
     C     QQRTCD        WHENEQ    '2'
     C                   SETON                                        57  90
     C                   ENDSL                                                  QQRTCD
     C*
     C                   ENDIF                                                  SCFOLD != *BLANKS
     C*
     C  N90SCALWR        IFNE      'Y'
     C     SCALWR        ANDNE     *BLANK
     C                   SETON                                        67  90
     C                   ENDIF                                                  SCALWR != Y/*BLANK
     C*
      /Free
             erscemal = *Off;
              If %Len(%trim(scemal)) > 0;
              // Check_Email is bound by copy so if it doesn't match with
              //  SPIGENERR, this program will need to be recompiled
                 erscemal = Check_Email(scemal:msgscemal);
                If erscemal = *On;
                   *IN90 = *On;
                EndIf;
              EndIf;
      /End-free
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* KNDO -
     C*================================================================
     C     KNDO          BEGSR
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C     SLKEY         CHAIN     PSL01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      SLUSRP        SCUSR
     C                   MOVE      SLUDSC        SCUDSC
     C                   Z-ADD     SLSECL        SCSECL
     C                   MOVE      SLF01         SCFL01
     C                   MOVE      SLF02         SCFL02
     C                   MOVE      SLF03         SCFL03
     C                   MOVE      SLF04         SCFL04
     C                   MOVE      SLF05         SCFL05
     C                   MOVE      SLF06         SCFL06
     C                   MOVE      SLF07         SCFL07
     C                   MOVE      SLF08         SCFL08
     C                   MOVE      SLF09         SCFL09
     C                   MOVE      SLF10         SCFL10
     C                   MOVE      SLF11         SCFL11
     C                   MOVE      SLF12         SCFL12
     C                   MOVE      SLF13         SCFL13
     C                   MOVE      SLF14         SCFL14
     C                   MOVE      SLF15         SCFL15
     C                   MOVE      SLF16         SCFL16
     C                   MOVE      SLF17         SCFL17
     C                   MOVE      SLF18         SCFL18
     C                   MOVE      SLF19         SCFL19
     C                   MOVE      SLF20         SCFL20
     C                   MOVE      SLF21         SCFL21
     C                   MOVE      SLF22         SCFL22
     C                   MOVE      SLF23         SCFL23
     C                   MOVE      SLF24         SCFL24
     C                   MOVE      SLALWR        SCALWR
     C                   MOVEL     SLEMAL        SCEMAL
     C** Access and default to Visual DARTS
     C                   SELECT
     C*
     C     SLVDAD        WHENEQ    'D'
     C                   MOVE      'Y'           SCVDTA
     C                   MOVE      'Y'           SCVDTD
     C*
     C     SLVDAD        WHENEQ    'Y'
     C                   MOVE      'Y'           SCVDTA
     C                   MOVE      ' '           SCVDTD
     C*
     C     SLVDAD        WHENEQ    ' '
     C                   MOVE      ' '           SCVDTA
     C                   MOVE      ' '           SCVDTD
     C                   ENDSL
     C*
     C                   MOVE      SLCODE        SCCODE
     C                   MOVEL     SLRMCD        SCRMCD
     C                   MOVE      SLCSID        SCCSID
     C                   MOVE      SLCCTF        SCCCTF
     C                   MOVE      SLCCIF        SCCCIF
     C                   MOVEL     SLFOLD        SCFOLD
     C                   MOVE      SLEDC         SCEDC
     C                   MOVE      SLDDC         SCDDC
     C                   MOVE      SLADC         SCADC
     C                   MOVE      SLDELC        SCDELC
     C                   MOVE      SLRACC        SCRACC
     C                   MOVE      SLSTAT        SCSTAT
     C*
     C                   ELSE                                                   *IN60 = *OFF
     C*
     C                   MOVE      *BLANK        SCRACC
     C                   MOVE      'Y'           SCFL01
     C                   MOVE      'Y'           SCFL02
     C                   MOVE      'Y'           SCFL03
     C                   MOVE      'Y'           SCFL04
     C                   MOVE      'Y'           SCFL05
     C                   MOVE      'Y'           SCFL06
     C                   MOVE      'Y'           SCFL07
     C                   MOVE      'Y'           SCFL08
     C                   MOVE      'Y'           SCFL09
     C                   MOVE      'Y'           SCFL10
     C                   MOVE      'Y'           SCFL11
     C                   MOVE      'Y'           SCFL12
     C                   MOVE      'Y'           SCFL13
     C                   MOVE      'Y'           SCFL14
     C                   MOVE      'Y'           SCFL15
     C                   MOVE      'Y'           SCFL16
     C                   MOVE      'Y'           SCFL17
     C                   MOVE      'Y'           SCFL18
     C                   MOVE      'Y'           SCFL19
     C                   MOVE      'Y'           SCFL20
     C                   MOVE      'Y'           SCFL21
     C                   MOVE      'Y'           SCFL22
     C                   MOVE      'Y'           SCFL23
     C                   MOVE      'Y'           SCFL24
     C                   MOVE      *BLANK        SCVDTA
     C                   MOVE      *BLANK        SCVDTD
     C*
     C                   MOVE      *BLANKS       SCRMCD
     C                   MOVE      *BLANKS       SCCODE
     C                   MOVE      *BLANKS       SCCSID
     C                   MOVE      'Y'           SCCCTF
     C                   MOVE      'Y'           SCCCIF
     C                   MOVE      *BLANKS       SCFOLD
     C                   MOVE      'Y'           SCEDC
     C                   MOVE      'Y'           SCDDC
     C                   MOVE      'Y'           SCADC
     C                   MOVE      'Y'           SCDELC
     C                   MOVE      'A'           SCSTAT
     C                   MOVE      *BLANKS       SCALWR
     C*
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C     SLKEY         CHAIN     PSL01                              60
     C*
     C                   MOVE      SCUSR         SLUSRP
     C                   MOVE      SCUDSC        SLUDSC
     C                   Z-ADD     SCSECL        SLSECL
     C                   MOVE      SCRACC        SLRACC
     C                   MOVE      SCFL01        SLF01
     C                   MOVE      SCFL02        SLF02
     C                   MOVE      SCFL03        SLF03
     C                   MOVE      SCFL04        SLF04
     C                   MOVE      SCFL05        SLF05
     C                   MOVE      SCFL06        SLF06
     C                   MOVE      SCFL07        SLF07
     C                   MOVE      SCFL08        SLF08
     C                   MOVE      SCFL09        SLF09
     C                   MOVE      SCFL10        SLF10
     C                   MOVE      SCFL11        SLF11
     C                   MOVE      SCFL12        SLF12
     C                   MOVE      SCFL13        SLF13
     C                   MOVE      SCFL14        SLF14
     C                   MOVE      SCFL15        SLF15
     C                   MOVE      SCFL16        SLF16
     C                   MOVE      SCFL17        SLF17
     C                   MOVE      SCFL18        SLF18
     C                   MOVE      SCFL19        SLF19
     C                   MOVE      SCFL20        SLF20
     C                   MOVE      SCFL21        SLF21
     C                   MOVE      SCFL22        SLF22
     C                   MOVE      SCFL23        SLF23
     C                   MOVE      SCFL24        SLF24
     C     SCVDTA        IFEQ      'Y'
     C*
     C     SCVDTD        IFEQ      'Y'
     C                   MOVE      'D'           SLVDAD
     C                   ELSE
     C                   MOVE      'Y'           SLVDAD
     C                   ENDIF                                                  SCVDTD = 'Y'
     C                   MOVE      SCALWR        SLALWR
     C                   MOVEL     SCEMAL        SLEMAL
     C*
     C                   ELSE
     C                   MOVE      *BLANK        SLALWR
     C                   MOVE      *BLANKS       SLEMAL
     C                   MOVE      *BLANK        SLVDAD
     C                   ENDIF                                                  SCVDTA = 'Y'
      /Free
       // Always default to iDARTS;
       SLvDaD = 'D';
      /End-free
     C*
     C                   MOVE      SCRMCD        SLRMCD
     C                   MOVE      SCCODE        SLCODE
     C                   MOVE      SCCSID        SLCSID
     C                   MOVE      SCCCTF        SLCCTF
     C                   MOVE      SCCCIF        SLCCIF
     C                   MOVE      SCFOLD        SLFOLD
     C                   MOVE      SCEDC         SLEDC
     C                   MOVE      SCDDC         SLDDC
     C                   MOVE      SCADC         SLADC
     C                   MOVE      SCDELC        SLDELC
     C*
     C                   MOVE      SCSTAT        SLSTAT
     C                   MOVE      Q1USER        SLUSER
     C                   MOVE      Q1PGM         SLPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SLDT
     C*
     C                   Z-ADD     DSTIME        SLTIME
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
      *
     C     *INKJ         CASEQ     *ON           KNDO                     90
     C                   ENDCS
      *
     C     *INKU         IFEQ      *ON
      * Change user status
     C                   SELECT
     C     SCSTAT        WHENEQ    *BLANK
     C                   MOVE      'A'           SCSTAT
     C     SCSTAT        WHENEQ    'A'
     C                   MOVE      'I'           SCSTAT
     C     SCSTAT        WHENEQ    'I'
     C                   MOVE      'A'           SCSTAT
     C                   ENDSL
      *
     C                   SETON                                            90
      *
     C                   ENDIF
      *
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      'C'           QQCALL
     C                   ENDIF
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   Z-ADD     0             @@RRN             9 0
     C                   MOVE      QQUSER        XXUSR
     C                   MOVE      QQUSER        SCUSR
     C                   MOVE      QQSEC         SCSECL
     C                   MOVEL     QQDESC        SCUDSC
     C*
     C     QQCALL        IFEQ      'D'
     C                   MOVE      QQCALL        @@MODE            1
     C                   ELSE
     C     SLKEY         CHAIN     PSL01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'C'           @@MODE
     C                   ELSE
     C                   MOVE      'A'           @@MODE
     C                   ENDIF                                                  QQUSER = *BLANKS
     C                   ENDIF                                                  QQCALL = 'D'
     C*
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C** Check System Control File Install Flags for this Client:
     C     'SY'          CHAIN     SSY01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C** Check for OV/400 installed
     C     SYOFIF        IFEQ      'Y'
     C                   MOVE      'Y'           XXOFIF            1            Installed
     C                   ELSE
     C                   MOVE      'N'           XXOFIF                         Installed
     C                   ENDIF                                                  END 'Y' SYOFIF
     C*
     C** Check for Visual DARTS installed
     C                   MOVE      *OFF          *IN64
     C*
     C     'SC'          CHAIN     SDR01                              60
     C     *IN60         IFEQ      *OFF
     C     DRFL10        IFEQ      'Y'
     C                   MOVE      *ON           *IN64
     C                   ENDIF                                                  DRFL01 = Y
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     SLKEY         CHAIN     PSL01                              99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    PSL01
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      SMSLP
     C                   MOVE      *ON           OUTFLG
     C                   ENDSR
