      /title WA062R - API to Create records for Return Fund Check Request                          *
     Hcopyright('Fiserv, Inc., 2008')                                                              *
     H option(*nodebugio: *srcstmt)                                                                *
     H debug(*yes)                                                                                 *
      *============================================================================================*
     * IF CALLED WITH A MODE = '1', GET THE NEXT RETURN FUND BATCH CONTROL NUMBER FROM WXRNP      *
     * AND RETURN THE BATCH # TO THE CALLING PROGRAM.                                             *
     * IF CALLED WITH A MODE = '2', CHAIN THE RRN PASSED INTO THE PROGRAM TO THE APPROPRIATE      *
     * FILE AND TRANSFER THE INFORMATION TO WXRTP.                                                *
     * FOR BMICO, WXRTP IS POPULATED FROM WFSDP                                                   *
     *                                                                                            *
     * CALLED FROM:                                                                               *
     *                                                                                            *
     * CALLS TO:                                                                                  *
     *                                                                                            *
      *  PROGRAMMER: C 183                                                                         *
      *  CREATED:    12/26/07                                                                      *
      *  PROJECT#:   81WZB                                                                         *
      *============================================================================================*
      *  REVISIONS:                                                                                *
      *                                                                                            *
      *============================================================================================*
     FWXRTP     IF A E             DISK
     FWXRNL     IF A E           K DISK
     FWMUPL     IF   E           K DISK
     FWFSDP     IF   E             DISK    USROPN
      *============================================================================================*
      * DATA STRUCTURES
      *============================================================================================*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     D                 DS
     D DSDTTM                        14  0
     D  DSTIME                        6  0 OVERLAY(DSDTTM)
     D  DSDATE                        8  0 OVERLAY(DSDTTM:7)
      *============================================================================================*
      * PROGRAM FIELDS
      *============================================================================================*
     D SRDTI           S              8  0
     D SRDTO           S             10
     D SRERR           S              1
     D SRFFMT          S              8
     D SRTFMT          S              8
     d ##co#           s                   like(RNCO#)
    d*=============================================================================================
    d* Procedure Prototypes
    d*=============================================================================================
     d wrtWXRNP81      pr
     d wrtWXRTP81      pr
      *============================================================================================*
      * PLIST'S
      *============================================================================================*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@MODE            1
     C                   PARM                    @@BAT#            9 0
     C                   PARM                    @@RRN#            9 0
      *============================================================================================*
      * KLIST'S
      *============================================================================================*
      *============================================================================================*
      * MAINLINE
      *============================================================================================*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *
     C     Q1USER        CHAIN     WMUPL
     C                   IF        %FOUND(WMUPL)
     C                   EVAL      ##CO# = UPCO#
     C                   ENDIF                                                  WMUPL
      *============================================================================================*
      *  Return next available batch number from WXRNP if @@mode = '1'
      *============================================================================================*
     c                   select
     C                   when      @@mode = '1'

     c                   select
     c                   when      cltid = '081'
     c                   callp     wrtWXRNP81
     c                   other
     c                   endsl

     c                   eval      *inlr = *on
     c                   return

      *============================================================================================*
      *  Populate WXRTP if @@mode = '2'
      *============================================================================================*
     c                   when      @@mode = '2'

     c                   select
     c                   when      cltid = '081'
     c                   callp     wrtWXRTP81
     c                   other
     c                   endsl

     c                   eval      *inlr = *on
     c                   return

     c                   other
     c                   eval      *inlr = *on
     c                   return
     c                   endsl
     p*===========================================================================
     p* wrtWXRNP81 - write WXRNP record for BMICO from WFSDP (Surplus Dividend)
     p*===========================================================================
     p wrtWXRNP81      b
     d wrtWXRNP81      pi

     d kkbat#          s                   like(RNBAT#) inz(999999999)

     c     rnky          klist
     c                   kfld                    ##co#

     c     rnky1         klist
     c                   kfld                    ##co#
     c                   kfld                    kkbat#

     c     rnky1         setgt     prn01
     c     rnky          readpe    prn01
     c                   if        not(%eof)
     c                   eval      @@bat# = RNBAT# + 1
     c                   else
     c                   eval      @@bat# = 1
     c                   endif                                                  NOT %EOF

     c                   eval      RNCO# = ##co#
     c                   eval      RNBAT# = @@BAT#
     c                   eval      RNBTYP = 'I'
     c                   eval      RNMTYP = 'M'
     c                   eval      RNPTYP = 'E'
     c                   eval      RNSDFL = 'Y'
     c                   eval      RNIPGM = Q1PGM
     c                   eval      RNPGM  = Q1PGM
     c                   eval      RNIUSR = Q1USER
     c                   eval      RNUSER = Q1USER
     c                   time                    dsdttm
     c                   eval      SRDTI  = DSDATE
     c                   CALL      'SRCVTDAT'    DTCYMD
     c                   MOVEL     SRDTO         RNIDT
     c                   MOVEL     SRDTO         RNDATE
     c                   eval      RNITIM = DSTIME
     c                   eval      RNTIME = DSTIME
     c                   write     prn01

     p wrtWXRNP81      e
     p*===========================================================================
     p* wrtWXRTP81 - write WXRTP record for BMICO from WFSDP (Surplus Dividend)
     p*===========================================================================
     p wrtWXRTP81      b
     d wrtWXRTP81      pi

     c                   open      wfsdp
     c     @@rrn#        chain     wfsdp
     c                   if        %found(wfsdp)
     c                   eval      RTCO#  = SDCO#
     c                   eval      RTBAT# = @@BAT#
     c                   eval      RTFND  = SDFND
     c                   eval      RTEMP# = SDEMP#
     c                   eval      RTDIV  = SDDIV
     c                   eval      RTEMPN = SDEMNM
     c                   eval      RTAGT# = SDAGT#
     c                   eval      RTAGTN = SDAGNM
     c                   eval      RTFYR  = SDFYR
     c                   eval      RTAMT  = SDCKAM
     c                   eval      RTSELC = 'Y'
     c                   eval      RTPTFL = 'E'
     c                   eval      RTBTYP = 'I'
     c                   eval      RTCUST = SDCUST
     c                   eval      RTSDFL = 'Y'
     c                   eval      RTIPGM = Q1PGM
     c                   eval      RTPGM  = Q1PGM
     c                   eval      RTIUSR = Q1USER
     c                   eval      RTUSER = Q1USER
     c                   time                    dsdttm
     c                   eval      SRDTI  = DSDATE
     c                   CALL      'SRCVTDAT'    DTCYMD
     c                   MOVEL     SRDTO         RTIDT
     c                   MOVEL     SRDTO         RTDATE
     c                   eval      RTITIM = DSTIME
     c                   eval      RTTIME = DSTIME
     c                   write     prt01
     c                   endif                                                  %FOUND(WFSDP)
     c                   close     wfsdp

     p wrtWXRTP81      e
