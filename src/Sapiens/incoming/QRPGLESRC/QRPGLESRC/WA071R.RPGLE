     * (C) Copyright FISERV Insurance Solutions, Inc., 2008
     *
    H/TITLE WA070R - Alternate Employer API
     *===============================================================*
      *  GIVEN:   co/fnd/emp/div/fyr
      *
     *===============================================================*
     * Revisions:                                                    *
     *                                                               *
     *===============================================================*

      * Group
     Fwmfnl     if   e           k disk

      * Employer Policy Years
     Fwdell     if   e           k disk

      * Rating State Code
     Fwmrsl     if   e           k disk

      * Reporting/Rating
     Fwdrrl     if   e           k disk

      * Endorsement form table
     Fwtenl     if   e           k disk
      *
     Fswcpf     if   e             disk
      *
     Fwmexl     if   e           k disk
      *
      * Employer Master
     Fwmeml     IF   E           K Disk
     F*
     FWDAEL     IF   E           K DISK
     F                                     RENAME(PAE01:PAE02)
     F                                     Infds(aeinfds)
     FWDAEP     O    E             DISK
      *=========================================================================
      *  Definition Specs

     D aeinfds         ds
     D  aerrn                397    400b 0

     D psds          esds                  extname(szq1p)

     D xxform          c                   const('WC000301A ')

     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26

     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8

     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)

      *=========================================================================
     *=========================================================================
     *  KLIST definitions
     *=========================================================================

     * Group

     c     fnky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd

     * Employer Policy Years

     c     elky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxemp#
     c                   kfld                    xxDIV
     c                   kfld                    xxfyr

     * Employer Master

     c     emky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxemp#
     c                   kfld                    xxdiv

     * Rate Set Code file

     c     rsky          KList
     c                   KFld                    xxco#
     c                   KFld                    elrtcd

     * Reporting Rating

     c     rrky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    rsjrst
     c                   kfld                    xxpfdt            7 0
      *
     c     rrky1         klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    rsjrst

     * Endorsement form table file

     c     enky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    x1form           15

     c*  System control file
     c     syky          klist
     c                   KFLD                    XXSYK             2

     C*  Alternate Employer file
     C     AEKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    x#div             5 0
     C*
     C     AEKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    x#div             5 0
     C                   KFLD                    xxseq#

     *=========================================================================
     *  PLIST definitions
     *=========================================================================

     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*

     * Entry

     c     *entry        plist
     c                   parm                    qqco#             3
     c                   parm                    qqfnd             3
     c                   parm                    qqemp#            9
     c                   parm                    qqDIV             5
     c                   parm                    qqfyr             3

     *=========================================================================
     *  Main
     *=========================================================================


     C                   call      'WA001R'
     C                   parm                    xxco#
     C                   parm                    xxfnd
     C                   parm                    ##conm           40
     C                   parm                    ##fnam           40
     C                   parm                    ##lvl             3 0
     C                   parm                    ##type            1
     C                   parm                    ##outq           10

     c                   move      qqco#         xxco#             3 0
     c                   move      qqfnd         xxfnd             3 0
     c                   move      qqemp#        xxemp#            9 0
     c                   move      qqdiv         xxdiv             5 0
     c                   move      qqfyr         xxfyr             3 0


     c     emky          chain     pem01

      * Initialize
    
      * Get Group info - PEO Flag
    
     c     fnky          chain     pfn01

     c                   if        (%found and fnpeof <> ' ')

     c                   read      swcpf                                  60
     c                   if        WCFL03 = 'Y'

      * Get Reporting/Rating info - PEO Output Control Flag

     C                   Z-ADD     *ZEROS        XXPFDT
     c     elky          chain     pel01
     c                   if        %found
     C*
     C                   Z-ADD     ELPFDT        XXPFDT

     c     rsky          chain     prs01
     c                   if        %found

     c     rrky          setgt     prr01
     c     rrky1         readpe    prr01

     c                   endif                                                  RSKY %found
     c                   endif                                                  ELKY %found

     c                   if        ((fnpeof = 'S')
     c                             or  (fnpeof = 'G') or (fnpeof = 'Y'))
    c                             and ((rrpgfl = '1') or (rrpgfl = '3'))

     c                             or

     c                             (fnpeof = 'M') and ((rrpgfl = '1')
    c                             or (rrpgfl = '2') or (rrpgfl = '3'))

     c                   movel     xxform        x1form
     c     enky          chain     pen01                              60
     c                   if        (*in60 = *off)
     c                   movel     enpgm2        xxpgm2           10

     c* When FNPEOF = 'M', ELEMP# Otherwise, 1.
     c* When FNPEOF = 'M', state reporting division Otherwise, 0.

     c                   move      xxemp#        ##emp#            9
     c                   move      xxdiv         ##div             5
     C
     c                   call      xxpgm2
     c                   parm                    qqco#
     c                   parm                    qqfnd
     c                   parm                    ##emp#
     c                   parm                    ##div
     c                   parm                    qqfyr
     c                   parm      '1'           ##mode            1
     c
     c                   if        (fnpeof = 'S')
     c                   move      '000000001'   ##emp#
     c                   move      '00000'       ##div

     c                   call      xxpgm2
     c                   parm                    qqco#
     c                   parm                    qqfnd
     c                   parm                    ##emp#
     c                   parm                    ##div
     c                   parm                    qqfyr
     c                   parm      '1'           ##mode            1
     c                   endif                                                  if (fnpeof = 'S')
     C
     c                   endif                                                  if (*in60 = *off)

     c     fnky          chain     pex01
     c                   if        %found
     c                   movel     exapcd        xxrmcd
     c                   endif

     C                   exsr      sraddrec

     c* Call SR00301S
     c                   call      'SR00301S'
     c                   parm                    qqco#
     c                   parm                    qqfnd
     c                   parm                    qqemp#
     c                   parm                    qqdiv
     c                   parm                    qqfyr
     c                   parm                    x1form
     c                   parm      'E'           xxmode            1
     c                   parm                    xxrrn             9 0
     c                   parm                    xxrmcd            5
     c                   parm                    ##emp#
     c                   parm                    ##div             5
     c                   parm                    ##outq
     C
     c                   endif                                                  FNPEO = 'S','G''Y'.
     c                   endif                                                  WCFL03 = 'Y'
     c                   endif                                                  FNKY %found

     c                   eval      *inlr = *on
     C*================================================================
     C* SRAaddRec - Write data base record
     C*================================================================
     C     SRAddRec      BEGSR
     C*
     C*
     C                   Z-ADD     0             X#DIV
     C     AEKY          SETGT     PAE02
     C     AEKY          READPE    PAE02                                  60
     C     *IN60         IFEQ      *OFF
     C     AESEQ#        ADD       1             XXSEQ#            5 0
     C                   ELSE
     C                   Z-ADD     1             XXSEQ#
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   CLEAR                   PAE02
     C*
     C                   Z-ADD     XXSEQ#        AESEQ#
     C*
     C                   Z-ADD     ELCO#         AECO#
     C                   Z-ADD     ELFND         AEFND
     C                   Z-ADD     ELEMP#        AEEMP#
     C                   Z-ADD     0             AEDIV
     C                   Z-ADD     ELFYR         AEFYR
     C*
     C                   MOVEL     EMNAME        AENAM1
     C*
     C                   IF        EMMPFL = 'M'
     C                   MOVEL     EMMAD1        AEADR1
     C                   MOVEL     EMMAD2        AEADR2
     C                   MOVEL     EMMCTY        AECTY
     C                   MOVEL     EMMST         AEST
     C                   MOVEL     EMMZIP        AEZIP
     C                   MOVEL     EMMST         AEEMST
     C                   ELSE
     C                   MOVEL     EMPAD1        AEADR1
     C                   MOVEL     EMPAD2        AEADR2
     C                   MOVEL     EMPCTY        AECTY
     C                   MOVEL     EMPST         AEST
     C                   MOVEL     EMPZIP        AEZIP
     C                   MOVEL     EMPST         AEEMST
     C                   ENDIF                                                   EMMPFL = 'M'
     C*
     C                   MOVEL     *BLANKS       AEPROJ
     C*
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
     C*
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
     C*
     C                   Z-ADD     LDEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AEEFDT
     C*
     C                   ELSE
     C*
     C                   Z-ADD     ELPFDT        AEEFDT
     C*
     C                   ENDIF                                                  LDENDA != 0
     C*
     C                   MOVEL     Q1USER        AEIUSR
     C                   MOVEL     Q1USER        AEUSER
     C                   MOVEL     Q1PGM         AEIPGM
     C                   MOVEL     Q1PGM         AEPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVEL     DSTIME        AEITM
     C                   MOVEL     DSTIME        AETIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AEIDT
     C                   MOVEL     SRDTO         AEDT
     C*
     C                   WRITE     PAE01
     C*
     C     AEKY1         CHAIN(N)  PAE02                              60
     C                   eval      xxrrn = aerrn
     C*
     C                   ENDSR
