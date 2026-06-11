     H*copyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H*StoneRiver is a trademark of StoneRiver, Inc.')
     H*
     H/TITLE W8005R - UDS Claim Import File Convert
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*   Copy the detail records from the flat files                 *
     H*   to the formatted files                                      *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   NONE                                                        *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* PROGRAMMER: C:189                                             *
     H* CREATED: 04/20/10               PROJECT: 114WWA               *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*   *IN60 - GENERAL CHAIN, DOWEQ                                *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 12/12/17 c:62 Ticket 51591(114)                               *
     H*               Don't use recipient claim#                      *
     H*                                                               *
     H*****************************************************************
     FWXU0P     IF   E           K DISK
     FWXU2P     IF   E           K DISK
     FWXUDP     O    E           K DISK
     FWXUFP     O    E           K DISK
      *
     D                 DS
     DDSDTTM                   1     12  0
     DDSTIME                   1      6  0
     DDSDATE                   7     12  0
     D*
     D UDPDATA         DS
     D XXRETY                         1    OVERLAY(UDPDATA)
     D XXINCO                         5    OVERLAY(UDPDATA : 2)
     D XXFLST                         2    OVERLAY(UDPDATA : 7)
     D XXFLCO                         2    OVERLAY(UDPDATA : 9)
     D XXCVCD                         6    OVERLAY(UDPDATA : 11)
     D XXPOL#                        20    OVERLAY(UDPDATA : 17)
     D XXICL#                        20    OVERLAY(UDPDATA : 37)
     D XXRCL#                        20    OVERLAY(UDPDATA : 57)
     D XXINL1                        30    OVERLAY(UDPDATA : 77)
     D XXINL2                        30    OVERLAY(UDPDATA : 107)
     D XXIAD1                        30    OVERLAY(UDPDATA : 137)
     D XXIAD2                        30    OVERLAY(UDPDATA : 167)
     D XXICTY                        25    OVERLAY(UDPDATA : 197)
     D XXINST                         2    OVERLAY(UDPDATA : 222)
     D XXIZCO                         9    OVERLAY(UDPDATA : 224)
     D XXLSDT                         8    OVERLAY(UDPDATA : 233)
     D XXEFDT                         8    OVERLAY(UDPDATA : 241)
     D XXEXDT                         8    OVERLAY(UDPDATA : 249)
     D XXCLM#                         5    OVERLAY(UDPDATA : 257)
     D XXCNL1                        30    OVERLAY(UDPDATA : 262)
     D XXCNL2                        30    OVERLAY(UDPDATA : 292)
     D XXCAD1                        30    OVERLAY(UDPDATA : 322)
     D XXCAD2                        30    OVERLAY(UDPDATA : 352)
     D XXCCIT                        25    OVERLAY(UDPDATA : 382)
     D XXCSTA                         2    OVERLAY(UDPDATA : 407)
     D XXCZCO                         9    OVERLAY(UDPDATA : 409)
     D XXCINI                         1    OVERLAY(UDPDATA : 418)
     D XXCID#                         9    OVERLAY(UDPDATA : 419)
     D XXTRCD                         3    OVERLAY(UDPDATA : 428)
     D XXTAMT                        11    OVERLAY(UDPDATA : 431)
     D XXSIGN                         1    OVERLAY(UDPDATA : 442)
     D XXCLCD                         2    OVERLAY(UDPDATA : 443)
     D XXRICD                         1    OVERLAY(UDPDATA : 445)
     D XXSIND                         1    OVERLAY(UDPDATA : 446)
     D XX2IFI                         1    OVERLAY(UDPDATA : 447)
     D XXTPA#                        30    OVERLAY(UDPDATA : 448)
     D XXLCL#                        30    OVERLAY(UDPDATA : 478)
     D XXICCD                         5    OVERLAY(UDPDATA : 508)
     D XXSOCD                         6    OVERLAY(UDPDATA : 513)
     D XXCRDT                         8    OVERLAY(UDPDATA : 519)
     D XXCBDT                         8    OVERLAY(UDPDATA : 527)
     D XXRPIN                         1    OVERLAY(UDPDATA : 535)
     D XXWICD                         3    OVERLAY(UDPDATA : 536)
     D XXWPOB                         3    OVERLAY(UDPDATA : 539)
     D XXWNOI                         3    OVERLAY(UDPDATA : 542)
     D XXWCAU                         3    OVERLAY(UDPDATA : 545)
     D XXWACT                         3    OVERLAY(UDPDATA : 548)
     D XXWTLS                         3    OVERLAY(UDPDATA : 551)
     D XXWTRE                         3    OVERLAY(UDPDATA : 554)
     D XXWTCD                         3    OVERLAY(UDPDATA : 557)
     D XXWTSE                         3    OVERLAY(UDPDATA : 560)
     D XXWVRI                         1    OVERLAY(UDPDATA : 563)
     D XXDOIN                        64    OVERLAY(UDPDATA : 564)
     D XXWCA#                        12    OVERLAY(UDPDATA : 628)
     D XXEWP#                        10    OVERLAY(UDPDATA : 640)
     D XXAPIN                         1    OVERLAY(UDPDATA : 650)
     D XXDPIN                         1    OVERLAY(UDPDATA : 651)
     D*
     D UFPDATA         DS
     D X1RETY                         1    OVERLAY(UFPDATA)
     D X1NAIC                         5    OVERLAY(UFPDATA : 2)
     D X1FLST                         2    OVERLAY(UFPDATA : 7)
     D X1FLCO                         2    OVERLAY(UFPDATA : 9)
     D X1ICL#                        30    OVERLAY(UFPDATA : 11)
     D X1RCL#                        20    OVERLAY(UFPDATA : 41)
     D X1CLM#                         5    OVERLAY(UFPDATA : 61)
     D X1ENDT                         8    OVERLAY(UFPDATA : 66)
     D X1NID#                         4    OVERLAY(UFPDATA : 74)
     D X1NLS#                         4    OVERLAY(UFPDATA : 78)
     D X1ENTX                      1000    OVERLAY(UFPDATA : 82)
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *=============================================================
      *  KLIST
      *=============================================================
      *
     C*
      *================================================================
      *  *LIKE DEFN
      *================================================================
     C*
     C*Move parameter values to numeric variables
     C
     C                   TIME                    DSDTTM
     C*
      *================================================================
      *  MAINLINE CODE START HERE
      *================================================================
     C*"----------------------
     C*" Read WXU0P records
     C*"----------------------
     C*
     C                   READ      PU001
     C                   DOW       NOT %eof(WXU0P)
     C*
      * Break the flat file data in to a data structure
     C*
     C                   CLEAR                   PUD01
     C                   MOVE      U0DATA        UDPDATA
     C*
      *  We will ignore the Header and Trailer records.
     C*
     C                   IF        XXRETY = 'A'  or   XXRETY = 'E' or
     C                             XXRETY = 'a'  or   XXRETY = 'e'
     C*
     C*
     C                   EXSR      MOVUDP
     C*
     C                   WRITE     PUD01
     C                   ENDIF
     C*
     C                   READ      PU001
     C                   ENDDO
     C*
     C*"----------------------
     C*" Read WXU2P records
     C*"----------------------
     C*
     C                   READ      PU201
     C                   DOW       NOT %eof(WXU2P)
     C*
      * Break the flat file data in to a data structure
     C*
     C                   CLEAR                   PUF01
     C                   MOVE      U2DATA        UFPDATA
     C*
      *  We will ignore the Header and Trailer records.
     C*
     C                   IF        X1RETY = 'F'  or   X1RETY = 'f'
     C*
     C                   EXSR      MOVUFP
     C*
     C                   WRITE     PUF01
     C                   ENDIF
     C*
     C                   READ      PU201
     C                   ENDDO
     C*
     C                   SETON                                        LR
      *===============================================================
      * MOVUDP- Move the data structure fields to file WXUDP fields
      *===============================================================
     C     MOVUDP        BEGSR
     C
     C                   EVAL      UDRETY = XXRETY
     C*
     C                   TESTN                   XXINCO               51
     C   51              MOVE      XXINCO        UDINCO
     C*
     C                   EVAL      UDFLST = XXFLST
     C*
     C                   TESTN                   XXFLCO               51
     C   51              MOVE      XXFLCO        UDFLCO
     C*
     C                   TESTN                   XXCVCD               51
     C   51              MOVE      XXCVCD        UDCVCD
     C*
     C                   EVAL      UDPOL# = XXPOL#
     C                   EVAL      UDICL# = XXICL#
     C*** Begin Chg *** 12/12/17 *******************************************
     C**********         EVAL      UDRCL# = XXRCL#
     C                   EVAL      UDRCL# = XXICL#
     C*** End   Chg *** 12/12/17 *******************************************
     C                   EVAL      UDINL1 = XXINL1
     C                   EVAL      UDINL2 = XXINL2
     C                   EVAL      UDIAD1 = XXIAD1
     C                   EVAL      UDIAD2 = XXIAD2
     C                   EVAL      UDICTY = XXICTY
     C                   EVAL      UDINST = XXINST
     C                   EVAL      UDIZCO = XXIZCO
     C*
     C                   TESTN                   XXLSDT               51
     C   51              MOVE      XXLSDT        UDLSDT
     C*
     C                   TESTN                   XXEFDT               51
     C   51              MOVE      XXEFDT        UDEFDT
     C*
     C                   TESTN                   XXEXDT               51
     C   51              MOVE      XXEXDT        UDEXDT
     C*
     C                   TESTN                   XXCLM#               51
     C   51              MOVE      XXCLM#        UDCLM#
     C*
     C                   EVAL      UDCNL1 = XXCNL1
     C                   EVAL      UDCNL2 = XXCNL2
     C                   EVAL      UDCAD1 = XXCAD1
     C                   EVAL      UDCAD2 = XXCAD2
     C                   EVAL      UDCCIT = XXCCIT
     C                   EVAL      UDCSTA = XXCSTA
     C                   EVAL      UDCZCO = XXCZCO
     C                   EVAL      UDCINI = XXCINI
     C*
     C                   TESTN                   XXCID#               51
     C   51              MOVE      XXCID#        UDCID#
     C*
     C                   TESTN                   XXTRCD               51
     C   51              MOVE      XXTRCD        UDTRCD
     C*
     C                   TESTN                   XXTAMT               51
     C   51              MOVE      XXTAMT        UDTAMT
     C*
     C* Add the sign to the transaction amount
     C
     C                   IF        XXSIGN = '-'
     C                   EVAL      UDTAMT = (UDTAMT * -1)
     C                   ENDIF
     C*
     C                   TESTN                   XXCLCD               51
     C   51              MOVE      XXCLCD        UDCLCD
     C*
     C                   EVAL      UDRICD = XXRICD
     C                   EVAL      UDSIND = XXSIND
     C                   EVAL      UD2IFI = XX2IFI
     C                   EVAL      UDTPA# = XXTPA#
     C                   EVAL      UDLCL# = XXLCL#
     C                   EVAL      UDICCD = XXICCD
     C                   EVAL      UDSOCD = XXSOCD
     C*
     C                   TESTN                   XXCRDT               51
     C   51              MOVE      XXCRDT        UDCRDT
     C*
     C                   TESTN                   XXCBDT               51
     C   51              MOVE      XXCBDT        UDCBDT
     C*
     C                   EVAL      UDRPIN = XXRPIN
     C                   EVAL      UDWICD = XXWICD
     C                   EVAL      UDWPOB = XXWPOB
     C                   EVAL      UDWNOI = XXWNOI
     C                   EVAL      UDWCAU = XXWCAU
     C                   EVAL      UDWACT = XXWACT
     C                   EVAL      UDWTLS = XXWTLS
     C                   EVAL      UDWTRE = XXWTRE
     C                   EVAL      UDWTCD = XXWTCD
     C                   EVAL      UDWTSE = XXWTSE
     C                   EVAL      UDWVRI = XXWVRI
     C                   EVAL      UDDOIN = XXDOIN
     C                   EVAL      UDWCA# = XXWCA#
     C*
     C                   TESTN                   XXEWP#               51
     C   51              MOVE      XXEWP#        UDEWP#
     C*
     C                   EVAL      UDAPIN = XXAPIN
     C                   EVAL      UDDPIN = XXDPIN
     C
     C                   ENDSR
      *===============================================================
      * MOVUFP- Move the data structure fields to file WXUFP fields
      *===============================================================
     C     MOVUFP        BEGSR
     C
     C                   EVAL      UFRETY = X1RETY
     C
     C                   TESTN                   X1NAIC               51
     C   51              MOVE      X1NAIC        UFNAIC
     C
     C                   EVAL      UFFLST = X1FLST
     C
     C                   TESTN                   X1FLCO               51
     C   51              MOVE      X1FLCO        UFFLCO
     C
     C                   EVAL      UFICL# = X1ICL#
     C*** Begin Chg *** 12/12/17 *******************************************
     C**********         EVAL      UFRCL# = X1RCL#
     C                   EVAL      UFRCL# = X1ICL#
     C*** End   Chg *** 12/12/17 *******************************************
     C
     C                   TESTN                   X1CLM#               51
     C   51              MOVE      X1CLM#        UFCLM#
     C
     C
     C                   TESTN                   X1ENDT               51
     C   51              MOVE      X1ENDT        UFENDT
     C
     C
     C                   TESTN                   X1NID#               51
     C   51              MOVE      X1NID#        UFNID#
     C
     C
     C                   TESTN                   X1NLS#               51
     C   51              MOVE      X1NLS#        UFNLS#
     C
     C                   EVAL      UFENTX = X1ENTX
     C
     C                   ENDSR
     C
