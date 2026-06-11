     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*
     H****************************************************************
     H*  F8402R - Update Production Files
     H*
     H*  DESCRIPTION: This pgm will update the production files with
     H*               Repriced data from Procura.
     H*
     H*  CREATED    : 12/18/2010  PROGRAMMER: 251  PROJECT: 146MCV
     H*
     H*  CALLED BY  : F8402C
     H*  CALLS TO   : None
     H*
     H*
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 04/10/18 C:079 Project: 114MJQ                                *
     H*                For client 114 remove S1 Medical Verbiage      *
     H*                                                               *
     H* 12/05/17 C:079 Support: 114/51483                             *
     H*                Check for non numeric EOB number               *
     H*                                                               *
     H* 10/25/17 C:079 Support: 114/50116                             *
     H*                If you dont get a hit on claim# try EOB#       *
     H*                Dont skip first record in FAZ3P                *
     H*                                                               *
     H* 05/23/17 C:180 Project: 114MFB                                *
     H*                For 114 change notes (dnote1 and dnote2)       *
     H*                to include subnetwork information.             *
     h*                e.g.                                           *
     h*                '....S1 Medical Network' to                    *
     H*                '....S1 Medical/X Network.  where X is the     *
     H*                subnetwork.                                    *
     H*                                                               *
     H* 02/20/17 C:079 Support: 45754/114                             *
     H*                For 114 change Procura/ONENET to S1 Medical    *
     H*                and the phone number to 484.880.4018           *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H*  09/03/15 C:212 Project: MC400                                *
     H*                 Procura ICD10 update                          *
     H*                                                               *
     H*  08/12/15 C:212 Project: 146MGI                               *
     H*                 FOr client 146, pend the hospital bills       *
     H*
     H*  08/26/14 C:307 Project: SY354                                *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H*  10/27/14 C:296 Project: 114MPZ                               *
     H*                 Load network code into EOB only if the PPO    *
     H*                 flag returned by procura indicates in network.*
     H*
     H*  09/25/14 C:296 Project: 114MPQ                               *
     H*                 Update EONSNT = 'R' in subroutine SRfile.     *
     H*
     H*  03/03/14 C:274 Project: 146MGB                               *
     H*                 Change client 146 to use Procura              *
     H*                 Change population of "EENOT1" & "EENOT2"      *
     H*                 to use "eontwk" instead of "CLTID"            *
     H*
     H*  11/07/13 C:296 Project: 114MID                               *
     H*                 Update file FDCAP, update adjudication        *
     H*                 code = 2 where it is 1.                       *
     H*
     H*  06/03/13 C:079 Support: 82491                                *
     H*                 For client 146 use different contact info     *
     H*                 Correct maintenance fields in FDDEP           *
     H*
     H*  04/17/13 C:284 Project: 114MID                               *
     H*                 Bills in PRP status if client is CSI. If the  *
     H*                 bill was processd thru claim chk, set flag so *
     H*                 that it will nt go back thru claim check.     *
     H*                                                               *
     H*  05/22/12 C:251 Project: 146MEZ
     H*                 Write detail EOB note containing Pricing Reason
     H*                 Code and Description
     H*
     H*  02/15/12 C:251 Support: 79231
     H*                 Change to not update EOUSER
     H*
     H*  02/11/11 C:079 Support: 75846
     H*                 Update values so that Provider Checks can
     H*                 be issued correctly in the batch
     H*
     H****************************************************************
     F*
     F* Procura Repriced Claim file
     FFAZ3L1    UF   E             DISK
     F*
     F* Transmission History record
     FFAZ9L1    UF   E           K DISK
     F*
     F* Export detail file
     FFAZ2L2    UF   E           K DISK
     F*
     F* Procura Return Header File
     FFAZ4P     IF A E             DISK
     F*
     F* Procura Return Detail File
     FFAZ5P     IF A E             DISK
     F*
     F* faeop by co/sub/eob
     FFAEOL3    UF   E           K DISK
     F*
     F* faeap by CO#/SUB#/FND/FYR/CASE/EOB#/RCN
     FFAEAL1    UF   E           K DISK
     F*
     F* fdcap by CO#/SUB#/FND/FYR/CASE/EOB#/Lin#
     FFDCAL19   uf   E           k DISK
     F*
     F* fddep by CO#/SUB#/FND/FYR/CASE/EOB#/DSEQ/SEQ#
     FFDDEL     IF A E           k DISK
     F*
     F* detail data
     FFDDAL1    uf   E           k DISK
     F*
     F* Holds Procura's messages
     FFAEEL     uf a E           k DISK
     F*
     F* EOB History
     FFAESP     IF A E             DISK
     F*
     F*** beg add *** 02/11/11 *********************************************
     FWMCIL     IF   E           K DISK    USROPN
     F*** end add *** 02/11/11 *********************************************
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     F*** Beg Add *** 04/17/13 *************************************************
     FFTBSL     IF   E           K DISK
     F*** End Add *** 04/17/13 *************************************************
     F*
     F*** Beg Add *** 11/07/13 *************************************************
     F* fdcap by CO#/SUB#/FND/FYR/CASE/EOB#/CPT4/DSEQ
     FFDCAL18   uf   E           k DISK    Rename(PCA01:PCA02)
     F*** End Add *** 11/07/13 *************************************************
     F***** Beg Add *** 08/12/15 *******************************************
     FNMNWL     IF   E           K DISK
     F***** End Add *** 08/12/15 *******************************************
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
     F*
     D*****************************************************************
     D/COPY SCOPYWW01I
     D*
     D*****************************************************************
     D* Program constants.
     D*****************************************************************
     D COMAPI          C                   CONST('FA007R')
     D xcReas          C                   CONST('Received from Procura')
     D TRANS           C                   CONST('Transaction ID:')
     D USERID          C                   CONST('PDSPGMR   ')
     D complete        C                   CONST('Completed by Procura upload')
     D*
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D  Z5DS         E DS                  EXTNAME(FAZ5P)
     D  Z4DS         E DS                  EXTNAME(FAZ4P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXCLM#                 1     50
     D  XXFND                  1      3
     D  XXFYR                  4      6
     D  XXCASE                 7     13
     D*
     D xxeob#          s             10
     D pos             s              2  0
     D len             s              2  0
     D xxtmp           s              9
     D xxcnt           s             11  0
     D totproc         s             11  0
     D prveob          s             10
     D errflg          s              1
     D err             s             45
     D*
     D xxlinar         s              1    dim(3)
     D xxtmpar         s              1    dim(9)
     D xxeobar         s              1    dim(9)
     D* ------------------------
     D* Procura's Repriced Data
     D*
     D                 DS                  INZ
     D*** Beg Chg *** 05/22/12 *********************************************
     D*** Beg Chg *** 09/03/15 *********************************************
     D  Z3DATA                 1   3000
     D*** Z3DATA                 1   1200
     D*** End Chg *** 09/03/15 *********************************************
     D*** End Chg *** 05/22/12 *********************************************
     D  cureob                 1     10
     D*
     D*** beg add *** 02/11/11 *********************************************
     D  xxtrcd                             like(citrcd)
     D  xxckst                             like(cists)
     D  kksys                              like(cisys)
     D  kkptyp                             like(ciptyp)
     D  kkwcf#                             like(cifnd)
     D ##wcif          s              1
     D ##mcif          s              1
     D ##wcin          s              1
     D ##ar            s              1
     D ##ntwk          s              1
     D ##ur            s              1
     D ##med           s              1
     D ##edi           s              1
     D ##wcdk          s              1
     D ##rhab          s              1
     D ##mss           s              1
     D ##fsch          s              1
     D ##prvb          s              1
     D ##npwc          s              1
     D ##os            s              1
     D ##arls          s              1
     D ##ccod          s              1
     D ##crvw          s              1
     D ##prom          s              1
     D ##mcrv          s              1
     D ##freq          s              1
     D ##crac          s              1
     D ##preo          s              1
     D ##parn          s              1
     D ##mgri          s              1
     D ##kcif          s              1
     D ##curs          s              1
     D ##pcib          s              1
     D ##clt#          s              1
     D*** end add *** 02/11/11 *********************************************
     D*** Beg Add *** 05/22/12 *********************************************
     D*** Beg Chg *** 06/10/13 *********************************************
     D*** xxStr           s            280
     D xxStr           s            290
     D*** End Chg *** 06/10/13 *********************************************
     D xxnote          s             70
     D xxchr           s              1
     D strpos          s              3  0
     D endpos          s              3  0
     D*** End Add *** 05/22/12 *********************************************
     D*** Begin Add *** 05/23/17 *******************************************
     D str350          s            350a   inz
     D intidx          s              3  0 inz
     D intStrPos       s              3  0 inz
     D intEndPos       s              3  0 inz

      *
      * Procedures
      *
     D Wrap70          pr            70
     D                              350
     D                                3  0
     D                                3  0
     D*** End   Add *** 05/23/17 *******************************************
     D*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@dcid
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTYYMD2       PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     CAKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##SUB#
     C                   KFLD                    kkFND
     C                   KFLD                    kkFYR
     C                   KFLD                    kkCASE
     C                   KFLD                    kkEOB#
     C                   KFLD                    kkLin#
     C*
     C*** Beg Add *** 11/07/13 *********************************************
     C     KeyFDCap      klist
     C                   kfld                    ##co#
     C                   kfld                    ##sub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkEOB#
     C*** End Add *** 11/07/13 *********************************************
     C*
     C     EAKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##SUB#
     C                   KFLD                    kkFND
     C                   KFLD                    kkFYR
     C                   KFLD                    kkCASE
     C                   KFLD                    kkEOB#
     C*
     C     EEKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##SUB#
     C                   KFLD                    kkFND
     C                   KFLD                    kkFYR
     C                   KFLD                    kkCASE
     C                   KFLD                    kkEOB#
     C                   KFLD                    kkseq#
     C*
     C     EEKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##SUB#
     C                   KFLD                    kkFND
     C                   KFLD                    kkFYR
     C                   KFLD                    kkCASE
     C                   KFLD                    kkEOB#
     C*
     C     EOKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##SUB#
     C                   KFLD                    kkEOB#
     C*
     C     DEKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##SUB#
     C                   KFLD                    kkFND
     C                   KFLD                    kkFYR
     C                   KFLD                    kkCASE
     C                   KFLD                    kkEOB#
     C                   KFLD                    kkdseq
     C                   KFLD                    kkseq#
     C*
     C     DEKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##SUB#
     C                   KFLD                    kkFND
     C                   KFLD                    kkFYR
     C                   KFLD                    kkCASE
     C                   KFLD                    kkEOB#
     C                   KFLD                    kkdseq
     C*
     C     DAKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##SUB#
     C                   KFLD                    kkFND
     C                   KFLD                    kkFYR
     C                   KFLD                    kkCASE
     C                   KFLD                    kkEOB#
     C                   KFLD                    kkdseq
     C*
     C*** beg add *** 02/11/11 *********************************************
     C*
     C     ciky          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    kkwcf#
     C                   KFLD                    kkptyp
     C                   KFLD                    kksys
     C*** end add *** 02/11/11 *********************************************
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    ##CO#
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    KKPRT             1
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C     Z9KY          KLIST
     C                   KFLD                    xxdcid
     C*
     C     Z2KY          KLIST
     C                   KFLD                    z5eob#
     C*
     C*** Beg Add *** 04/17/13 *************************************************
     C     BSKY          KLIST
     C                   KFLD                    ##co#
     C                   KFLD                    esblst
     C                   KFLD                    esstcd
     C*** End Add *** 04/17/13 *************************************************
     C***** Beg Add *** 08/12/15 *******************************************
     C     NWKY          KLIST
     C                   KFLD                    ##co#
     C                   KFLD                    eontwk
     C***** End Add *** 08/12/15 *******************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    EOdt          xxDT
     C     *LIKE         DEFINE    EOtime        xxTime
     C     *LIKE         DEFINE    EOuser        user
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    CAlin#        kklin#
     C     *LIKE         DEFINE    Deseq#        kkseq#
     C     *LIKE         DEFINE    Deseq#        hdrseq
     C     *LIKE         DEFINE    EONDIS        xxppo
     C     *LIKE         DEFINE    EONDIS        xxppotot
     C     *LIKE         DEFINE    EANSNT        status
     C     *LIKE         DEFINE    z9dcid        @@dcid
     C     *LIKE         DEFINE    z9dcid        xxdcid
     C*================================================================
     C*                MAIN SUBROUTINE
     C*================================================================
     C*
     C                   exsr      pgmstr
     C*
     C                   read      pz301                                  98
     C*
     C                   if        *in98 = *off
     C*
     C                   exsr      SrHead
     C*
     C                   exsr      SrDetail
     C                   endif
     C*
     C* Update Transmission file
     C*
     C                   exsr      SrZ9p
     C*
     C* if no error on report
     C                   if        xxcnt = *zeros
     C                   except    norec
     C                   else
     C                   except    totcnt
     C                   endif
     C*
     C                   exsr      PGMEND
     C*
     C*================================================================
     C* Srhead - Write Header record to FAZ4P
     C*================================================================
     C     SrHead        BEGSR
     C*
     C*** move the first record to header field
     C*
     C                   clear                   PZ401
     C                   movel     z3data        z4ds
     C*
     C* Set date sent as current date
     C                   EVAL      Z4DTSN = xxdt
     C                   EVAL      Z4FIL# = 1
     C*
     C                   write     pz401
     C*
     C                   endsr
     C*================================================================
     C* SrDetail - write to detail line and calculate
     C*================================================================
     C     SrDetail      BEGSR
     C*
     C* Move the detail records to detail file.
     C*
       //*** Beg Add *** 10/25/17 *****************************************
       // Start reading at the beginning of the file
      /Free
                         setll 1 pz301;
      /End-Free
       //*** End Add *** 10/25/17 *****************************************
     C                   read      pz301                                  98
     C                   eval      prveob  = *blanks
     C*
     C                   Dow       *in98 = *off
     C*
     C                   if        prveob <> cureob  and errflg = 'N'
     C                             and prveob <> *blanks
     C                   exsr      SrFile
     C                   eval      xxppotot = *zeros
     C*
     C                   endif
     C*
     C* Move the flat file to detail record format
     C                   clear                   PZ501
     C                   movel     z3data        z5ds
     C                   movel     z3prfl        z5prfl
     C                   exsr      SrRtv
     C*
     C                   eval      errflg = 'N'
     C                   exsr      Errck
     C*
     C                   if        errflg = 'Y'
     C* Skip processing and read next record
     C                   eval      prveob =  cureob
     C                   read      pz301                                  98
     C                   iter
     C                   endif
     C*
     C** Determine if any PPO reductions was made
     C*
     C                   if        z5ppo <> *blanks
     C                   move      z5ppo         xxppo
     C                   else
     C                   eval      xxppo = *zeros
     C                   endif
     C*
     C                   exsr      Srcalc
     C*
     C*
     C* write to  Detail file FAZ5P record as processed
     C                   eval      z5prfl = 'P'
     C                   eval      z5dtsn = z4dtsn
     C                   eval      z5fil# = z4fil#
     C                   write     pz501
     C*
     C* write to  flat file FAZ3P record as processed
     C                   eval      z3prfl = 'P'
     C                   update    pz301
     C*
     C                   eval      totproc = totproc + 1
     C*
     C* Get the EOB and look it up in FAZ2P, update Processed flag as 'Returned'
     C     z2ky          setll     pz201
     C                   read      pz201                                  98
     C*
     C                   dow       *in98 = *off
     C*
     C** Updating the related record in FAZ2P where the bill is still
     C** in 'S' status.
     C*
     C                   if        z5eob# = z2eob# and z5lin# = z2lin#
     C                             and z2prfl ='S'
     C*
     C                   eval      z2prfl = 'R'
     C                   update    pz201
     C                   leave
     C                   endif
     C*
     C                   read      pz201                                  98
     C                   enddo
     C*
     C                   eval      prveob =  cureob
     C                   read      pz301                                  98
     C*
     C                   enddo
     C*
     C                   if        prveob <> *blanks and errflg = 'N'
     C                   exsr      SrFile
     C                   eval      xxppotot = *zeros
     C                   endif
     C*
     C                   endsr
     C*================================================================
     C* errck - Check for error on EOB in detail line
     C*================================================================
     C     Errck         BEGSR
     C*
     C                   eval      err = *blanks
     C                   eval      status = *blanks
     C*
     C** Read FAEAP record for EOB status
     C*
     C     eaky          chain(N)  pea01
     C                   if        %found
     C*
     C* EOB if not in sent status, then error
     C*
     C                   if        EANSNT <> 'S'
     C*
     C                   select
     C                   when      EANSNT = 'R'
     C                   eval      err = 'Already Returned from Procura'
     C                   other
     C                   eval      err='EOB was not in "Sent to Procura Status"'
     C                   endsl
     C*
     C                   eval      status = EANSNT
     C                   eval      xxcnt = xxcnt + 1
     C                   eval      errflg = 'Y'
     C*
     C                   endif                                                       EANSNT <> 'S'
     C*
     C                   else                                                        eob not found
     C*
     C                   eval      err = 'EOB not found'
     C                   eval      status = *blanks
     C                   eval      xxcnt = xxcnt + 1
     C                   eval      errflg = 'Y'
     C                   endif                                                       FAEAP endif
     C*
     C                   if        errflg = 'Y'
     C*
     C** Write to detail record
     C*
     C                   eval      z5prfl = 'E'
     C                   eval      z5dtsn = z4dtsn
     C                   eval      z5fil# = z4fil#
     C*** Beg Add *** 12/05/17 ***************************************
     C                   if        kkEOB# <> 999999999
     C*** End Chg *** 12/05/17 ***************************************
     C                   write     pz501
     C*** Beg Add *** 12/05/17 ***************************************
     C                   Endif
     C*** End Chg *** 12/05/17 ***************************************
     C*
     C* write to  flat file FAZ3P record as error
     C                   eval      z3prfl = 'E'
     C                   update    pz301
     C*
     C** Write to error report
     C                   if        (prveob <> cureob)
     C                   exsr      SrWrtRep
     C                   endif
     C*
     C                   endif
     C*
     C                   ENDSR
     C*================================================================
     C* SrWrtRep - Write error report when either bill not found or not in "Sent" status
     C*================================================================
     C     SrWrtRep      BEGSR
     C*
     C*
     C   OA              EXCEPT    HEAD
     C   OA              SETOFF                                       OA
     C                   EXCEPT    DET
     C*
     C                   ENDSR
     C*** beg add *** 02/11/11 *********************************************
     C*================================================================
     C* srgetxref - Get the WCMS cross reference group
     C*================================================================
     C     Srgetxref     BEGSR
     C*
     C                   MOVE      ##CO#         #1CO#
     C*
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM      *BLANKS       #1WCF#            3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C*
     C     ##RTCD        IFEQ      *BLANKS
     C                   MOVE      #1WCF#        KKWCF#
     C                   ELSE
     C                   Z-ADD     *ZEROS        KKWCF#
     C                   ENDIF
     C*
     C                   endsr
     C*================================================================
     C* srgetchkval - Determine transaction code and status
     C*================================================================
     C     Srgetchkval   BEGSR
     C*
     C* Default the transcation and status code
     C*
     C                   eval      xxtrcd = 'MP'
     C                   eval      xxckst = 'PD'
     C*
     C* If WCMS is installed then get the group xref and load wmcip fields
     C*
     C                   if        ##wcif = 'Y'
     C*
     C                   exsr      srgetxref
     C*
     C                   eval      kkptyp = 'P'
     C                   eval      kksys  = eosys
     C*
     C     ciky          chain     pci01
     C*
     C                   if        %found
     C*
     C                   eval      xxtrcd = citrcd
     C                   eval      xxckst = cists
     C*
     C                   endif
     C*
     C                   endif                                                  ##wcif = Y
     C*
     C                   ENDSR
     C*** end add *** 02/11/11 *********************************************
     C*================================================================
     C* srfile  - Update FAEAP, FAEOP,FAEEP,FAESP for the whole bill
     C*================================================================
     C     SrFile        BEGSR
     C*
     C* update FAEOP
     C*
     C     eoky          chain     peo01
     C                   if        %found
     C*
     C***** Beg Add *** 08/12/15 *******************************************
     C* read PP network file
     C     nwky          chain     pnw01
     C***** End Add *** 08/12/15 *******************************************
     C*** beg add 02/11/11 *************************************************
     C                   exsr      srgetchkval
     C*** Beg Chg *** 04/17/13 *********************************************
     C                   if        cltid  <> '114'
     C*** End Chg *** 04/17/13 *********************************************
     C                   eval      eotrcd = xxtrcd
     C*** beg add 02/11/11 *************************************************
     C*** Beg Chg *** 04/17/13 *********************************************
     C                   endif
     C*** End Chg *** 04/17/13 *********************************************
     C*
     C                   eval      eondis = eondis + xxppotot
     C                   eval      eopaid = eopaid - xxppotot
     C*** Beg Chg *** 04/17/13 *********************************************
     C                   if        cltid  = '114'
     C*** Beg Add *** 11/07/13 *********************************************
     C                   exsr      SRupdADJcde
     C*** End Add *** 11/07/13 *********************************************
     C                   eval      eoblst = 'P'
     C                   eval      eocrne = 'N'
     C                   else
     C*** End Chg *** 04/17/13 *********************************************
     C                   eval      eoblst = 'C'
     C*** Beg Chg *** 04/17/13 *********************************************
     C                   endif
     C*** End Chg *** 04/17/13 *********************************************
     C*** Beg Add *** 06/03/13 *********************************************
     C                   select
     C*
     C                   when      cltid = '146'
     C*** Beg Add *** 03/03/14 *********************************************
     C*** Beg Del *** 10/27/14 *********************************************
     C*****                        or cltid = '114'
     C*** End Del *** 10/27/14 *********************************************
     C*** End Add *** 03/03/14 *********************************************
     C*** Beg Chg *** 03/03/14 *********************************************
     C******             eval      eontwk = 'RMS'
     C                   eval      eontwk = 'PROC'
     C*** End Chg *** 03/03/14 *********************************************
     C*
     C                   other
     C*** End Add *** 06/03/13 *********************************************
     C*** Beg Chg *** 03/03/14 *********************************************
     C*****              eval      eontwk = 'PROC'
     C                   eval      eontwk = 'RMS '
     C*** End Chg *** 03/03/14 *********************************************
     C*** Beg Add *** 06/03/13 *********************************************
     C                   endsl
     C*** End Add *** 06/03/13 *********************************************
     C*** Beg Add *** 10/27/14 *********************************************
      /Free
        select;
        when cltid = '114';
           if z5inet = 'Y';
              EOntwk = 'PROC';
           else;
              EOntwk = *blanks;
           endif;
        endsl;
      /End-Free
     C*** End Add *** 10/27/14 *********************************************
     C*** Beg Chg *** 04/17/13 *********************************************
     C                   if        cltid  <> '114'
     C*** End Chg *** 04/17/13 *********************************************
     C                   eval      eochk# = eoeob#
     C*** Beg Chg *** 04/17/13 *********************************************
     C                   endif
     C*** End Chg *** 04/17/13 *********************************************
    C*
     C*** Beg Del *** 02/15/12 *********************************************
     C*******            movel(P)  q1user        eouser
     C*** End Del *** 02/15/12 *********************************************
     C                   movel(P)  q1pgm         eopgm
     C                   z-add     xxdt          eodt
     C                   z-add     xxtime        eotime
     C*** Beg Add *** 09/25/14 *********************************************
      /Free
        if cltid = '114' or cltid = '129';
           EOnSnt = 'R';
        else;
           EOnSnt = *blanks;
        endif;
      /End-Free
     C*** End Add *** 09/25/14 *********************************************
     C***** Beg Add *** 08/12/15 *******************************************
      /Free
        if cltid = '146' and EObtyp = 'H' and
           NWfl01 = 'Y' and NW10A3 <> *blanks;
           EOblst = 'P';
           EOnSnt = 'R';
           exsr SRupdADJcde;
        endif;
      /End-Free
     C***** End Add *** 08/12/15 *******************************************
     C                   update    peo01
     C                   endif
    C*
     C     eaky          chain     pea01
     C                   if        %found
     C*** Beg Chg *** 04/17/13 *********************************************
     C                   if        cltid  <> '114'
     C*** End Chg *** 04/17/13 *********************************************
     C*** beg add 02/11/11 *************************************************
     C                   eval      eackst = xxckst
     C*** beg add 02/11/11 *************************************************
     C*** Beg Chg *** 04/17/13 *********************************************
     C                   endif
     C*** End Chg *** 04/17/13 *********************************************
     C*
     C                   eval      eandis = eandis + xxppotot
     C                   eval      eapaid = eapaid - xxppotot
     C                   eval      eansnt = 'R'
     C                   z-add     xxdt          eanpdt
     C                   z-add     xxdt          eatrdt
     C                   eval      eaepfl = *blanks
     C*
    C*
     C                   movel(P)  q1user        eauser
     C                   movel(P)  q1pgm         eapgm
     C                   z-add     xxdt          eadt
     C                   z-add     xxtime        eatime
     C                   update    pea01
     C                   endif
     C*
     C                   if        xxppotot > *zeros
     C*
     C     eeky          setgt     pee01
     C     eeky1         readpe    pee01                                  99
     C*
     C                   if        *in99 = *off
     C                   eval      hdrseq = eeseq# + 1
     C                   else
     C                   eval      hdrseq =  1
     C                   endif                                                  Record in FAEEP
     C* write FAEEP
     C                   clear                   pee01
     C                   move      ##co#         eeco#
     C                   move      ##sub#        eesub#
     C                   move      kkfnd         eefnd
     C                   move      kkfyr         eefyr
     C                   move      kkcase        eecase
     C                   move      kkeob#        eeeob#
     C                   move      eoss#         eess#
     C                   z-add     eoacdt        eeacdt
     C                   z-add     hdrseq        eeseq#
     C                   eval      eenot1= 'Reimbursement calculated according +
     C                             to network referenced above; for'
    C*
     C*** Beg Add *** 06/03/13 *********************************************
     C                   select
    C*
     C*** Beg Chg *** 03/03/14 *********************************************
     C******             when      cltid = '146'
     C                   when      eontwk = 'RMS '
     C*** End Chg *** 03/03/14 *********************************************
     C                   eval      eenot2 = 'questions regarding PPO reductions+
     C                             , please call Rising Medical'
     C*
     C                   other
     C*** End Add *** 06/03/13 *********************************************
     C                   eval      eenot2 = 'questions regarding PPO reductions+
     C                             , please call Procura at'
     C*** Beg Add *** 06/03/13 *********************************************
     C                   endsl
     C*** End Add *** 06/03/13 *********************************************
     C*** Beg Add *** 02/20/17 *********************************************
     C                   if        cltid = '114'
     C                   eval      eenot2 = 'questions regarding PPO reductions+
     C                             , please call S1 Medical at'
     C                   endif
     C*** End Add *** 02/20/17 *********************************************
    C*
    C* Load maintenance fields
     C                   movel     'A'           eestat
     C                   movel     q1user        eeuser
     C                   movel     q1pgm         eepgm
     C                   movel     q1user        eeiusr
     C                   movel     q1pgm         eeipgm
    C*
     C                   z-add     xxdt          eedt
     C                   z-add     xxdt          eeidt
     C                   eval      eetime = xxtime
     C                   eval      eeitim = xxtime
     C*
     C*** Beg Add *** 04/10/18 ********************************************
     C                   If        cltId <> '114'
     C*** End Add *** 04/10/18 ********************************************
     C                   write     pee01
     C*** Beg Add *** 04/10/18 ********************************************
     C                   Endif
     C*** End Add *** 04/10/18 ********************************************
     C*
     C* write  another record to FAEEP
     C*
     C                   clear                   pee01
     C                   eval      hdrseq = hdrseq + 1
     C                   move      ##co#         eeco#
     C                   move      ##sub#        eesub#
     C                   move      kkfnd         eefnd
     C                   move      kkfyr         eefyr
     C                   move      kkcase        eecase
     C                   move      kkeob#        eeeob#
     C                   move      eoss#         eess#
     C                   z-add     eoacdt        eeacdt
     C                   z-add     hdrseq        eeseq#
     C*** Beg Add *** 06/03/13 *********************************************
     C                   select
    C*
     C*** Beg Chg *** 03/03/14 *********************************************
     C******             when      cltid = '146'
     C                   when      eontwk = 'RMS '
     C*** End Chg *** 03/03/14 *********************************************
     C                   eval      eenot1 = 'Services at: 1.866.274.7464.'
     C*
     C                   other
     C*** End Add *** 06/03/13 *********************************************
     C                   eval      eenot1 = '1.877.461.3750.'
     C*** Beg Add *** 06/03/13 *********************************************
     C                   endsl
     C*** End Add *** 06/03/13 *********************************************
     C*** Beg Add *** 02/20/17 *********************************************
     C                   if        cltid = '114'
     C                   eval      eenot1 = '1.484.880.4018'
     C                   endif
     C*** End Add *** 02/20/17 *********************************************
     C                   eval      eenot2 = *blanks
    C*
    C* Load maintenance fields
     C                   movel     'A'           eestat
     C                   movel     q1user        eeuser
     C                   movel     q1pgm         eepgm
     C                   movel     q1user        eeiusr
     C                   movel     q1pgm         eeipgm
    C*
     C                   z-add     xxdt          eedt
     C                   z-add     xxdt          eeidt
     C                   eval      eetime = xxtime
     C                   eval      eeitim = xxtime
     C*
     C                   If        cltId <> '114'
     C*** End Add *** 04/10/18 ********************************************
     C                   write     pee01
     C*** Beg Add *** 04/10/18 ********************************************
     C                   Endif
     C*** End Add *** 04/10/18 ********************************************
     C*
     C                   endif                                                  Z5PPO > 0
     C*
     C* write FAESP
     C*
     c                   clear                   pes01
     C                   move      ##co#         esco#
     C                   move      ##sub#        essub#
     C                   move      kkfnd         esfnd
     C                   move      kkfyr         esfyr
     C                   move      kkcase        escase
     C                   move      kkeob#        eseob#
     C                   move      eoss#         esss#
     C                   z-add     eoacdt        esacdt
     C*** Beg Chg *** 04/17/13 *********************************************
     C                   if        cltid  = '114'
     C                   eval      esblst = 'P'
     C                   eval      esstcd = 'PRP'
     C     bsky          chain     pbs01
     C                   if        %found(ftbsl)
     C                   eval      esreas = bsdesc
     C                   endif
     C                   else
     C*** End Chg *** 04/17/13 *********************************************
     C                   eval      esblst = 'C'
     C                   eval      esreas='Bill/EOMB completed by Procura Netw+
     C                                     ork'
     C*** Beg Chg *** 04/17/13 *********************************************
     C                   endif
     C*** End Chg *** 04/17/13 *********************************************
     C***** Beg Add *** 08/12/15 *******************************************
      /Free
        if cltid = '146' and EObtyp = 'H' and
           NWfl01 = 'Y' and NW10A3 <> *blanks;
           ESblst = 'P';
           ESstcd = NW10A3;
           Chain bsky pbs01;
           if %found(ftbsl);
              ESreas = bsdesc;
           endif;
        endif;
      /End-Free
     C***** End Add *** 08/12/15 *******************************************
    C*
    C* load maintenance fields
     C                   movel     'A'           esstat
     C                   movel     q1user        esuser
     C                   movel     q1pgm         espgm
     C                   movel     q1user        esiusr
     C                   movel     q1pgm         esipgm
     C                   z-add     xxdt          esdt
     C                   z-add     xxdt          esidt
     C                   eval      estime = xxtime
     C                   eval      esitim = xxtime
    C*
     C                   write     pes01
     C*
     C                   ENDSR
     C*================================================================
     C* SrCalc - PPO reductions amount to be included in file FDCAP,FDDAP,FDDEP
     C*================================================================
     C     SrCalc        BEGSR
     C*
     C* Keeping running total for PPO for the bill
     C                   eval      xxppotot = xxppotot + xxppo
     C*
     C* calculate for FDCAP
     C
     C     caky          chain     pca01
     C                   if        %found
     C*
     C* determine PPO reductions for each line item
     C*
     C                   eval      candis = candis + xxppo
     C                   eval      capaid = capaid - xxppo
     C                   eval      caflag = *blanks
     C                   z-add     caeob#        cachk#
     C*
     C                   MOVEL(P)  Q1USER        CAUSER
     C                   MOVEL(P)  Q1PGM         CAPGM
     C                   Z-ADD     xxTIME        CATIME
     C                   Z-ADD     xxDT          CADT
     C*
     C                   update    PCA01
     C*
     C                   eval      kkdseq = cadseq
     C*
     C* Write EOB note to FDDEP
     C*
     C                   if        xxppo > *zeros
     C*
     C     deky          setgt     pde01
     C     deky1         readpe    pde01                                  99
     C                   if        *in99 = *off
     C                   eval      hdrseq = deseq# + 1
     C                   else
     C                   eval      hdrseq =  1
     C                   endif                                                  Record in FDDEP
     C*
     C                   CLEAR                   PDE01
     C*
     C                   Z-ADD     CACO#         DECO#
     C                   Z-ADD     CASUB#        DESUB#
     C                   Z-ADD     CAFND         DEFND
     C                   Z-ADD     CAFYR         DEFYR
     C                   Z-ADD     CACASE        DECASE
     C                   MOVEL     CASS#         DESS#
     C                   Z-ADD     CAACDT        DEACDT
     C                   Z-ADD     CAEOB#        DEEOB#
     C                   Z-ADD     CADSEQ        DEDSEQ
     C                   Z-ADD     hdrseq        DESEQ#
     C*
     C                   eval      DEEOB = *blanks
     C                   eval      denot1 = 'PPO Reductions made according +
     C                                       to ' +%trim(z5ntnm)+' Network.'
      *** Beg Delete *** 05/23/17 ****************************************
     C*** Beg Add *** 02/20/17 *******************************************
     C***                   if        cltid = '114'
     C***                   eval      denot1 = 'PPO Reductions made according +
     C***                                       to S1 Medical Network.'
     C***                   endif
     C*** End Add *** 02/20/17 *******************************************
      *** End Delete *** 05/23/17 ****************************************
      /Free
        //*** Begin Add 05/23/17 ********************************************
        If cltid = '114';
           intStrPos = *zeros;
           intEndPos = *zeros;
           If z5ntnm = *blanks;
           //*** Beg Del *** 04/10/18 ********************************
           //*denot1 = 'PPO Reductions made according to S1 Medical Network.';
           //*** End Del *** 04/10/18 ********************************
           Else;
           //*** Beg Chg *** 04/10/18 ********************************
           //*** str350 =  'PPO Reductions made according to S1 Medical/' +
           //***       %trim(Z5ntnm) + ' Network.';
              str350 =  'PPO Reductions made according to ' +
                       %trim(Z5ntnm) + ' Network.';
           //*** End Chg *** 04/10/18 ********************************
              For intidx = 1 to 5;
                 Select;
                 When intidx = 1;
                    denot1 = Wrap70(Str350:intStrPos:intEndPos);
                 When intidx = 2;
                    denot2 = Wrap70(Str350:intStrPos:intEndPos);
                 Other;   //add more indexes if needed, up to 5
                    Leave;
                 Endsl;
              Endfor;
           Endif;
        Endif;
        //*** End   Add 05/23/17 ********************************************
      /End-Free
     C*
     C                   eval      DESTAT = 'A'
     C*** Beg Chg *** 06/14/13 *******************************************
     C*****************  eval      DEUSER = CAUSER
     C*****************  MOVEL(P)  CAPGM         DEPGM
     C*****************  Z-ADD     CATIME        DETIME
     C*****************  Z-ADD     CADT          DEDT
     C*****************  MOVEL(P)  CAUSER        DEIUSR
     C*****************  MOVEL(P)  CAPGM         DEIPGM
     C*****************  Z-ADD     CAITIM        DEITIM
     C*****************  Z-ADD     CAIDT         DEIDT
     C                   eval      DEUSER = Q1USER
     C                   MOVEL(P)  Q1PGM         DEPGM
     C                   Z-ADD     xxTIME        DETIME
     C                   Z-ADD     xxDT          DEDT
     C                   MOVEL(P)  Q1USER        DEIUSR
     C                   MOVEL(P)  Q1PGM         DEIPGM
     C                   Z-ADD     xxtime        DEITIM
     C                   Z-ADD     xxdt          DEIDT
     C*** End Chg *** 06/14/13 *******************************************
     C                   MOVEL(P)  'U'           DEESRC
     C                   MOVEL(P)  'Y'           DEPRNT
     C                   WRITE     PDE01
     C*** Beg Add *** 05/22/12 **************************************************
      /Free
        //  Beg Add *** 04/17/13 ****************************************** ****
       if   z5rcod <> *blanks or z5rcds <> *blanks;
        //  End Add *** 04/17/13 ****************************************** ****
         Exsr SrWrtNote ;
        //  Beg Add *** 04/17/13 ****************************************** ****
       endif;
        //  End Add *** 04/17/13 ****************************************** ****
      /end-Free
     C*** End Add *** 04/17/13 *************************************************
     C*** End Add *** 05/22/12 **************************************************
     C*
     C                   endif                                                      FDCAP rcd fnd
     C                   endif                                                      xxppo > 0
     C*
     C                   if        xxppo > *zeros
     C* calculate for FDDAP
     C     daky          chain     pda01
     C                   if        %found
     C*
     C* determine PPO reductions for each line item
     C*
     C                   eval      dandis = dandis + xxppo
     C                   eval      dapaid = dapaid - xxppo
     C*
     C                   MOVEL(P)  Q1USER        dAUSER
     C                   MOVEL(P)  Q1PGM         dAPGM
     C                   Z-ADD     xxTIME        dATIME
     C                   Z-ADD     xxDT          dADT
     C*
     C                   update    pda01
     C                   endif                                                      FDDAP rcd fnd
     C                   endif                                                      xxppo > 0
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C*** Beg Add *** 04/17/13 *********************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*** End Add *** 04/17/13 *********************************************
     C*** beg add *** 02/11/11 *********************************************
    C*
     C                   call      'SA001R'
     C                   parm      *blank        ##wcif
     C                   parm      *blank        ##mcif
     C                   parm      *blank        ##wcin
     C                   parm      *blank        ##ar
     C                   parm      *blank        ##ntwk
     C                   parm      *blank        ##ur
     C                   parm      *blank        ##med
     C                   parm      *blank        ##edi
     C                   parm      *blank        ##wcdk
     C                   parm      *blank        ##rhab
     C                   parm      *blank        ##mss
     C                   parm      *blank        ##fsch
     C                   parm      *blank        ##prvb
     C                   parm      *blank        ##npwc
     C                   parm      *blank        ##os
     C                   parm      *blank        ##arls
     C                   parm      *blank        ##ccod
     C                   parm      *blank        ##crvw
     C                   parm      *blank        ##prom
     C                   parm      *blank        ##mcrv
     C                   parm      *blank        ##freq
     C                   parm      *blank        ##crac
     C                   parm      *blank        ##preo
     C                   parm      *blank        ##parn
     C                   parm      *blank        ##mgri
12   C                   parm      *blank        ##kcif
12   C                   parm      *blank        ##curs
12   C                   parm      *blank        ##pcib
12   C                   parm      *blank        ##clt#
     C*
     C                   if        ##wcif = 'Y'
     C                   open      wmcil
     C                   end
     C*
     C*** end add *** 02/11/11 *********************************************
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        xxTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         xxDT
     C*
 001 C                   MOVEL(P)  USERID        USER
     C                   z-add     777           ##CO#
     C                   CALL      COMAPI        COMPL                  99
     C*
     C                   eval      totproc = *zeros
     C                   eval      xxppotot = *zeros
     C                   eval      xxppo   = *zeros
     C                   eval      xxcnt   = *zeros
     C                   MOVE      *ALL'_'       LINE             81
     C                   eval      kkseq# = *hival
     C*
     C                   exsr      SrRte
     C                   except    HEAD
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       kkPrt = '0';
       Chain orKy POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   EVAL      *INLR = *ON
     C*
     C                   ENDSR
     C*================================================================
     C* SrZ9p - WRITE  Transmission History File (FAZ9P).
     C*================================================================
     C     SrZ9p         BEGSR
     C*
     C                   MOVEL(P)  @@dcid        xxdcid
     C*
     C     xxdcid        CHAIN     PZ901                              95
     C     *IN95         IFEQ      *OFF
     C*
     C                   eval      z9prfl = 'Y'
     C** count of total records sent
     C                   eval      z9#rcd = totproc + xxcnt
     C                   MOVEL(P)  Q1USER        Z9USER
     C                   MOVEL(P)  Q1PGM         Z9PGM
     C                   Z-ADD     xxTIME        Z9prtm
     C                   Z-ADD     xxTIME        Z9time
     C                   z-add     xxDT          Z9prdt
     C                   z-add     xxDT          Z9dt
     C*
     C                   UPDATE    PZ901
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* SrRtv  - get EOB,Line number from character to numeric
     C*================================================================
     C     SrRtv         BEGSR
     C* Initialize the arrays
     C                   movea     *blanks       xxlinar
     C                   movea     *blanks       xxeobar
     C                   movea     *blanks       xxtmpar
     C                   eval      kklin# = *zeros
     C                   eval      kkeob# = *zeros
     C                   eval      xxtmp  = *blanks
     C*
     C* get Line number in right justified numeric format
     C*
     C                   movea     z5lin#        xxlinar
     C                   eval      len = %len(%trim(z5lin#))
     C                   eval      pos = 9
     C*
     C                   dow       len <> 0
     C                   move      xxlinar(len)  xxtmpar(pos)
     C                   eval      pos = pos-1
     C                   eval      len = len -1
     C                   enddo
     C*
     C                   movea     xxtmpar       xxtmp
     C                   move      xxtmp         kklin#
     C*
     C* clear temp variables
     C                   movea     *blanks       xxtmpar
     C                   eval      xxtmp  = *blanks
     C* get EOB number in right justified numeric format
     C*
     C*** Beg Chg *** 12/05/17 ************************************
     C********
     C********           movea     z5eob#        xxeobar
     C********           eval      len = %len(%trimr(z5eob#))
     C********           eval      pos = 9
     C********
     C********           dow       len <> 0
     C********           move      xxeobar(len)  xxtmpar(pos)
     C********           eval      pos = pos-1
     C********           eval      len = len -1
     C********           enddo
     C********
     C********           movea     xxtmpar       xxtmp
     C********           move      xxtmp         kkeob#
      /Free
                         Monitor;
                         kkEOB# = %Dec(Z5eob#:9:0);
                         On-Error;
                         kkEOB# = *hival;
                         Endmon;
      /End-free
     C*** End Chg *** 12/05/17 ************************************
     C*
     C** Move key fields
     C                   eval      xxclm# = z5clm#
     C                   move      xxfnd         kkfnd
     C                   move      xxfyr         kkfyr
     C                   move      xxcase        kkcase
        //*** Beg Add *** 10/25/17 *************************************
      /Free
        // If the claim number has been changed use the EOB#
                         Chain(n) eaKy pea01;
                         If Not %Found(faeal1);
                            Chain(n) eoKy peo01;
                            If %Found(faeol3);
                               ##Co# = EOco#;
                               ##Sub# = EOsub#;
                               kkFnd = EOfnd;
                               kkFyr = EOfyr;
                               kkCase = EOcase;
                               kkEOB# = EOeob#;
                            Endif;
                         Endif;
      /End-Free
        //*** End Add *** 10/25/17 *************************************
     C                   ENDSR
     C*================================================================
     C* SrRte  - Routing information for Error report
     C*================================================================
     C     SrRte         BEGSR
     C*
     C*   GET COMPANY, USER, INFO.
     C*
     C     COKY          CHAIN     PCO01                              60
     C     UPKY          CHAIN     PUP01                              60
     C*
     C*   Output Routing - detail report 0
     C*
     C                   MOVE      '0'           KKPRT
     C     ORKY          CHAIN     POR01                              60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C* Left Justify Title in Field
     C*
     C     ORTITL        IFNE      *BLANKS                                      FIELD
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Route to:
     C*
     C                   MOVEL(P)  UPUSNM        ORDIS1
     C                   MOVEL(P)  *BLANKS       ORDIS2
     C                   MOVEL(P)  *BLANKS       ORDIS3
     C                   MOVEL(P)  *BLANKS       ORDIS4
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   ENDIF
     C*
     C* Center Company name in display field
     C*
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        CONAME
     C*
     C                   ENDSR                                                  SRRTE
     C*** Beg Add *** 05/22/12 **********************************************
      /Free
       //======================================================================
       // SrWrtNote -  Write detail EOB note with Pricing Reason Code
       //              and Description
       //======================================================================

       BegSr SrWrtNote;

          xxStr = %trimr(Z5RCOD) + ': '+ %trim(Z5RCDS);
          strpos = 0 ;
          endpos = 0 ;
          denot1 = *blanks;
          denot2 = *blanks;

          Exsr SrWrap;
          denot1 = xxnote;

          Exsr SrWrap;
          denot2  = xxnote;

          if denot1 <> *blanks or denot2 <> *blanks;
           exsr  SrWrtDe1;
           exsr  SrWrap;
           if  xxnote = *blanks;
             leavesr;
           else;
             denot1 = xxnote;
             exsr SrWrap;
             denot2 = xxnote;
             exsr SrWrtDe1;
           endif;

          endif;

       Endsr ;

       //==============================================================
       // SrWrap -  Wrapping Logic of notes into 70 chars
       //==============================================================
       BegSr SrWrap;

         xxchr = *blanks;
         xxnote = *blanks;
         strpos = endpos + 1;
         endpos = strpos + 70;

         dow  endpos <> 0 and endpos >= strpos;
           xxchr = %subst(xxstr: endpos : 1);
           if  xxchr = *blanks;
            xxnote =%subst(xxstr:strpos:Endpos-strpos);
            leave;
           else;
            endpos = endpos -1;
           endif;
         enddo;

       Endsr;

       //==============================================================
       //   SrWrtDe1 -  Write to FDDEP
       //==============================================================
       BegSr SrWrtDe1;

           setgt  deky   pde01;
           readpe deky1  pde01;
           if not %eof;
            hdrseq = deseq# + 1;
           else;
            hdrseq =  1;
           endif;

           clear PDE01;

           DECO#  =  CACO#;
           DESUB# =  CASUB#;
           DEFND  =  CAFND;
           DEFYR  =  CAFYR;
           DECASE =  CACASE;
           DESS#  =  CASS#;
           DEACDT =  CAACDT;
           DEEOB# =  CAEOB#;
           DEDSEQ =  CADSEQ;
           DESEQ# =  hdrseq;
           DEEOB = *blanks;

           DESTAT = 'A';
       //*** Beg Chg *** 06/14/13 ****************************************
       //==DEUSER = CAUSER;
       //==DEPGM = CAPGM;
       //==DETIME = CATIME;
       //==DEDT   = CADT;
       //==DEIUSR = CAUSER;
       //==DEIPGM = CAPGM;
       //==DEITIM = CAITIM;
       //==DEIDT  = CAIDT;
           DEUSER = Q1USER;
           DEPGM = Q1PGM;
           DETIME = xxTIME;
           DEDT   = xxDT;
           DEIUSR = Q1USER;
           DEIPGM = Q1PGM;
           DEITIM = xxTIME;
           DEIDT  = xxDT;
       //*** End Chg *** 06/14/13 ****************************************
           DEESRC = 'U';
           DEPRNT = 'Y';

           WRITE  PDE01;

        Endsr;

      /End-Free
     C*** End Add *** 05/22/12 **********************************************
     C*** Beg Add *** 11/07/13 **********************************************
      /Free
       begsr SRupdADJcde;
         chain keyFDCap pca02;
         if %found(fdcal18);
           doW not %eof(fdcal18);
             if cacscd = '1' or cacscd = '01' or cacscd = '001';
               cacscd = '2';
               causer = Q1USER;
               capgm  = Q1PGM;
               catime = xxTIME;
               cadt   = xxDT;
               update  pca02;
             endif;
             reade keyFDCap pca02;
           enddo;
         endif;
       endSr;
      /End-Free
     C*** End Add *** 11/07/13 **********************************************
      *=====================================================================
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             ORDIS1              60
     O**********E            ROUTE          1
     O**********             ORDIS2              60
     O**********E            ROUTE          1
     O**********             ORDIS3              60
     O**********E            ROUTE          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
      *
     O          E            ROUTE          1 59
     O                                         +001 '(c)'
     O                       *YEAR             +001
     O                                         +001 'StoneRiver, Inc.'
     O                                         +001 'All rights reserved.'
     O          E            ROUTE          1
     O                                         +001 'StoneRiver is a'
     O                                         +001 'trademark of'
     O                                         +001 'StoneRiver, Inc.'
      *
     O          E            HEAD           1 03
     O                       DSTIME               8 '  :  :  '
     O                       CONAME              60
     O                       DSDATE        Y     82
      *
     O          E            HEAD           2
     O                       Q1PGM               10
     O                                           45 'Error Report'
     O                                           78 'Page:'
     O                       PAGE          Z     82
     O          E            HEAD           0
     O                       LINE              +001
     O          E            HEAD           2
     O                                         +006 'EOMB#'
     O                                         +012 'Error Description'
     O                                         +028 'Status'
     O          E            DET            1
     O                       KKEOB#            +002
     O                       err               +012
     O                       status            +003
      *
     O          E            TOTCNT         2
     O                       LINE              +001
      *
     O          E            TOTCNT         2
     O                                           25 'Total Records in Error:'
     O                       XXCNT         Z   +001
     O          E            NOREC          2
     O                                           45 '*** No error found in u'
     O                                              'ploaded Bills ***'
      *** Begin Add *** 05/23/17 ********************************************
      *--------------------------------------------------------
      * Wrap text (up to 350 characters) using complete words
      * into 70 character fields.
      *
      *   @@Note   - Note string    (Required)
      *   @@Start  - Start position (Required)
      *   @@End    - End position   (Required)
      *--------------------------------------------------------

     P Wrap70          B
     D Wrap70          PI            70
     D  @@Note                      350
     D  @@Start                       3  0
     D  @@End                         3  0
      *
     D  xxnote         S             70    inz
      /Free
         @@Start = @@End  + 1;
         @@End   = @@Start + 70;

         If @@End > 350;
            @@End = 350;
         Endif;

         Dow @@End  <> 0 and @@End  >= @@Start;
           if %subst(@@Note:@@End:1) = *blanks;
            xxnote =%subst(@@Note:@@Start:@@End-@@Start);
            leave;
           else;
            @@End = @@End -  1;
           endif;
         enddo;
         Return xxnote;
      /End-Free
     PWrap70           E
      *** End   Add *** 05/23/17 ********************************************
