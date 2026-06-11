    D**********************************************************************
    D*  Revisions:
    D*        02/06/12 C:191 Project: 114IAC
     D*                       Add Conditional exit points in KEDO
    D**********************************************************************
      /if not defined(CONVERT)
     D ol              s             80    dim(14)
     D fl              s             80    dim(14)
     D vo              s              1    dim(99)
     D vf              s              1    dim(24)
    D**
     D ##ofd           ds          3500
     D dbfds           ds
     D  dbrrn                397    400b 0
     D dbfds2          ds
     D  dbrrn2               397    400b 0
     D dbfds3          ds
     D  dbrrn3               397    400b 0
     D dbfds4          ds
     D  dbrrn4               397    400b 0
     D dbfds5          ds
     D  dbrrn5               397    400b 0
     D dbfds6          ds
     D  dbrrn6               397    400b 0
     D dbfds7          ds
     D  dbrrn7               397    400b 0
     D dbfds8          ds
     D  dbrrn8               397    400b 0
     D dbfds9          ds
     D  dbrrn9               397    400b 0
     D psds          esds                  extname(szq1p)
     D fids01        e ds                  extname(szq2p)
    D*
     D rclcmd          c                   const('RCLRSC *CALLER')
     D pdsnam          c                   const('StoneRiver, Inc.')
     D wwcons          c                   const('Work with')
     D cdcons          c                   const('Confirm Delete of')
     D sscons          c                   const('Subset')
    D**
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##data          s             40
     D ##of            s            100
     D ##pgm           s             10
     D ##wwmo          s              1
     D @cont           s              1
     D @inc            s              7  0
     D @incc           s              7  0
     D @rcds           s              3  0
     D @readc          s              7  0
     D @readr          s              5  0
     D @reads          s              7  0
     D autorf          s              1
     D count1          s              3  0
     D dumnum          s              2
     D dumofd          s             30
     D firsti          s              1
     D fl#1            s              3  0
     D fl#2            s              3  0
     D found           s              1
     D hd#             s              3  0
     D i               s              2  0
     D info#           s              1  0
     D l               s              1  0
     D lastrr          s              4  0
     D meetss          s              1
     D of#             s              3  0
     D ol#1            s              3  0
     D ol#2            s              3  0
     D outflg          s              1
     D rstpgm          s              1
     D scount          s              5  0
     D scuser          s             10
     D selmod          s              1
     D sfkey           s              5  0
     D sfrrn1          s              4  0
     D sfrrn2          s              4  0
     D sl1flg          s              1
     D sngitm          s              1
     D sv95            s              1
     D test1l          s              1
     D test1r          s              1
     D vi              s              3  0
     D view#           s              1  0
     D xxmis           s              3
     D xxnmbr          s              2
     D xxopfn          s              1
     *******************************************************************
     ******** Add by Profound Logic for Dynamic Function Keys **********
     D AV_KEYS_DS      ds                  qualified
     D ARRAY                         81    dim(14)
     D KEYS                        1134    OVERLAY(AV_KEYS_DS)
     *******************************************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D*
     D/copy sPrDate
     D/copy sPrMessage
     D/copy sPrRunCmd
     D/copy sPrString
     D/copy sPrWrkWth
      /Endif
    C*
    D*---------------------------------------------------------------------
    C*  MAINLINE (INTERNAL) INDICATOR REFERENCE SUMMARY
    C*
    C*  01 - Used on the PAGEDOWN keyword
    C*  02 - Used on the PAGEUP keyword
    C*  03 - Used to condition SFLCLR
    C* N03 - Used to condition SFLDSPCTL
    C*  04 - Used to condition SFLDSP
    C*  05 - Used to condition DSPATR(RI) on the selection field
    C* N06 - Used to condition DSPATR(PC) on the position to field
    C*  07 - Used to condition DSPATR(PC) on the selection field
    C*  08 - Used to condition SFLNXTCHG
    C*  09 - Reserved for maintenance/inquiry conditions
    C* N10 - Used to condition SFLEND (on message subfile)
    C*  11 - Used to indicate the list is subsetted
    C*  12 - Used to condition SFLDROP/SFLFOLD
    C*  13 - Used to condition SFLCLR (on confirm delete)
    C* N13 - Used to condition SFLDSPCTL (on confirm delete)
    C*  14 - Used to condition SFLDSP (on confirm delete)
    C*  15 - Used to condition SFLEND (on confirm delete)
    C*  21 - These indicators are used for the associated view #
    C* thru
    C*  29 - 21=View #1, 22=View #2, etc.
    C*  71 - These indicators are to used as error indicators for
    C* thru  the various screens. They will set off after
    C*  89 - displaying each input capible screen.
    C*  90 - Used as general purpose error indicator
    C*  91 - Used to display CPF5203; roll up/down past last record
    C*  92 - Used to condition SFLMSGID for more than one opt 1
    C*  95 - Used to condition SFLEND; also used to indicate EOF and
    C*       BOF on the file being read.
    C*  99 - Used for misc purpose; error on calls, on CHAINs
    C*
    C****************************************************************
    C*
    C*  MAINLINE (INTERNAL) SUBROUTINE REFERENCE SUMMARY
    C*
    C*  BLDLNS - Builds options and functions lines
    C*  CHKERR - Checks position of blank on selection; checks for
    C*           processing on deleted item.
    C*  CHKSFM - Check subfile mode for fold/truncate; if mode = '0'
    C*           then mode is fold, otherwise truncated
    C*  CLRGET - Clears subfile; get new set of records
    C*  CLRSF1 - Clears subfile; initializes relative record fields
    C*  DLTEXE - Displays confirm delete subfile; processes records
    C*  ENTRDO - Processes Enter key
    C*  GETCOF - Gets company and fund names if possible; otherwise
    C*           get PDS name for company name
    C*  GETPGT - Get program titles for all screens
    C*  GETRDB - Gets a page of records (backwards)
    C*  GETRDS - Gets a page of records
    C*  KCDO   - Process F3; end of program
    C*  KEDO   - Process F5; refreshes subfile
    C*  KFDO   - Process F6; add records
    C*  KIDO   - Process F9; change info
    C*  KKDO   - Process F11; change view
    C*  KLDO   - Process F12; cancel program
    C*  KMDO   - Process F13; user parameters
    C*  KRDO   - Process F17; subset selection
    C*  KXDO   - Process F23; more options
    C*  KYDO   - Process F24; more keys
    C*  LOADSF - Load item into subfile
    C*  OPT1   - Process option 1; select item
    C*  OPT2   - Process option 2; change item
    C*  OPT3   - Process option 3; copy item
    C*  OPT4   - Process option 4; delete item
    C*  OPT5   - Process option 5; display item
    C*  PGDNDO - Process page down; get another page in subfile
    C*  PGUPDO - Process page up; get page for subfile
    C*  READCK - Checks the number of reads; calls SRRDCCKR if necess
    C*  REFRSR - Refreshes the item in the subfile from data base
    C*  SF1CHK - Read subfile changes; checks for errors
    C*  SF1EXE - Process subfile changes
    C*  SNDSSM - Send subsetted list message if necessary
    C*  UPDOFA - Update values in the option/function array
    C*  UPDSF1 - Updates subfile record when item is deleted
    C*
    C****************************************************************
    C*  MAINLINE (EXTERNAL) SUBROUTINE REFERENCE SUMMARY
    C*
    C*  CHKSSM - Used to set indicator 11 for subset message
    C*  ERRCK1 - Used to determine errors on the subfile selection
    C*  ERRCK2 - Used to determine errors on the subset selection
    C*  INZSST - Used to initialize the subset selection fields
    C*  KEYINZ - Used to initialize key values when program starts
    C*  KEYSET - Used to set key values when using position to
    C*  KEYSFL - Used to set key values from subfile when paging
    C*  MVDBSC - Used to move data base fields to screen fields
    C*  OTHFKS - Used to process other function keys
    C*  OTHOPT - Used to process other options
    C*  PGMEND - Used for any end of program processing
    C*  PGMSTR - Used for any start of program processing
    C*  READ   - Used to read next record from file
    C*  READP  - Used to read previous record from file
    C*  SETGT  - Used to set greater than on file
    C*  SETLL  - Used to set lower limits on file
    C*  SF1UPD - Used to set subfile fields when item is deleted.
    C*  SLCTRD - Used to move fields for selected records
    C*  SUBSET - Used to determine if the record read should be placed
    C*           in the subfile.
    C*
    C****************************************************************
    C*
     C                   move      *zeros        q2lsfr
     C                   clear                   xxopfn
     C                   clear                   xxnmbr
     C                   clear                   xxmis
     C                   eval      info# = 1
     C                   eval      view# = 1
     C                   eval      @inc = 1000
     C                   eval      @incc = @inc
     C                   move      *off          autorf
     C                   move      *off          sngitm
     C                   move      *off          selmod
     C                   move      *off          *in06
     C                   movel     q1user        scuser
     C                   exsr      getcof
     C                   exsr      keyinz
     C                   exsr      pgmstr
     C                   exsr      gethd#
     C                   exsr      bldlns
     C                   exsr      getpgt
     C                   exsr      inzsst
     C                   exsr      clrget
     C                   callp     rmvmsg
     C                   exsr      sndssm
    C*
B01  C                   dou       rstpgm = *off
     C                   move      *off          rstpgm
    C*
B02  C                   dou       *inkc = *on
     C                             and vf(3) = *on
     C                             or outflg = *on
     C                   move      *off          outflg
     C                   if        *in90 = *off
     C                   write     msgsflc
     C                   write     scr01
     C                   endif
     C                   exfmt     scr01c
     C                   movea     '00000'       *in(71)
     C                   movea     '00000'       *in(76)
     C                   movea     '00000'       *in(81)
     C                   movea     '000000'      *in(86)
     C                   movea     '0000'        *in(91)
B03  C                   if        *inkc = *off
     C                             or vf(3) = *off
     C                   exsr      chkvfk
B04  C                   if        *in90 = *off
     C                   exsr      chksfm
B05  C                   if        q2iaid = '1'
    C*                    EXSR ERRCKC
B06  C                   if        *in90 = *off
     C                   exsr      entrdo
E06  C                   endif
X05  C                   else
     C     *in01         caseq     *on           pgdndo
     C     *in02         caseq     *on           pgupdo
     C     *inke         caseq     *on           kedo
     C     *inkf         caseq     *on           kfdo
     C     *inki         caseq     *on           kido
     C     *inkk         caseq     *on           kkdo
     C     *inkl         caseq     *on           kldo
     C     *inkm         caseq     *on           kmdo
     C     *inkr         caseq     *on           krdo
     C     *inkx         caseq     *on           kxdo
     C     *inky         caseq     *on           kydo
     C                   cas                     othfks
     C                   endcs
E05  C                   endif
E04  C                   endif
E03  C                   endif
B03  C                   if        *inkc = *on
     C                             and vf(3) = *on
     C                             and selmod = *on
     C                   move      *off          *inkc
E03  C                   endif
E02  C                   enddo
     C                   exsr      pgmend
E01  C                   enddo
     C                   exsr      kcdo
    C*
     C                   return
    C*================================================================
    C*  BLDLNS - Builds options and functions lines
    C*================================================================
     C     bldlns        begsr
     C                   eval      ol#1 = ol#1 - ol#1
     C                   eval      ol#2 = ol#2 - ol#2
     C                   eval      fl#1 = fl#1 - fl#1
     C                   eval      fl#2 = fl#2 - fl#2
     C                   movea(p)  of            ##of
     C                   movea(p)  ofd           ##ofd
     C                   clear                   ##wwmo
B01  C                   if        q1prms > 0
     C                   move      @@call        ##wwmo
E01  C                   endif
     C                   call      'SROPTFNC'                           99
     C                   parm                    ##wwmo
     C                   parm                    ##of
     C                   parm                    ##ofd
     C                   parm                    ol
     C                   parm                    fl
     C                   parm                    vo
     C                   parm                    vf
     C                   parm      q1pgm         ##pgm
     *******************************************************************
     ******** Add by Profound Logic for Dyamic Function Keys ***********
      /If Defined (WEB)
     C                   Eval      AV_KEYS_DS.ARRAY = fl
     C                   Eval      AV_KEYS =  AV_KEYS_DS.KEYS
      /Endif
     *******************************************************************
     C                   exsr      kxdo
     C                   exsr      kydo
     C                   endsr
    C*================================================================
    C*  CHKERR - Checks position of blank on selection; checks for
    C*           processing on deleted item.
    C*================================================================
     C     chkerr        begsr
     C     ' '           check     scsel         l
     C                   subst(p)  scsel:l       scsel
     C                   movel     scsel         test1l
     C                   move      scsel         test1r
     C                   eval      of# = of# - of#
B01  C                   if        test1r = *blank
     C                   testn                   test1l               99  99
B02  C                   if        *in99 = *off
     C                   eval      *in90 = *on
     C                   eval      *in93 = *on
E02  C                   endif
     C                   if        *in90 = *off
     C                   move      test1l        of#
     C                   endif
X01  C                   else
     C                   testn                   scsel                999999
B02  C                   if        *in99 = *off
     C                   eval      *in90 = *on
     C                   eval      *in93 = *on
E02  C                   endif
     C                   if        *in90 = *off
     C                   move      scsel         of#
     C                   endif
E01  C                   endif
B01  C                   if        *in90 = *off
     C                             and of# < 1
     C                   eval      *in90 = *on
     C                   eval      *in93 = *on
E01  C                   endif
B01  C                   if        *in90 = *off
     C                             and vo(of#) = *off
     C                   eval      *in90 = *on
     C                   eval      *in93 = *on
E01  C                   endif
B01  C                   if        *in90 = *off
     C                             and scdbrr = *zeros
     C                   eval      *in90 = *on
     C                   eval      *in93 = *on
E01  C                   endif
     C                   endsr
    C*================================================================
    C*  CHKSFB - Check subfile records (backward) for non-deleted
    C*           record, then use for setting keys
    C*================================================================
     C     chksfb        begsr
B01  C                   if        *in04 = *on
     C     sfkey         chain     scr01s                             99
B02  C                   dow       *in99 = *off
     C                             and scdbrr = *zeros
     C                   eval      sfkey = sfkey - 1
     C     sfkey         chain     scr01s                             99
E02  C                   enddo
E01  C                   endif
B01  C                   if        *in99 = *off
     C                             and *in04 = *on
     C                   eval      *in08 = *on
     C                   update    scr01s
     C                   exsr      keysfl
X01  C                   else
     C                   exsr      keyinz
E01  C                   endif
     C                   endsr
    C*================================================================
    C*  CHKSFF - Check subfile records (forward) for non-deleted
    C*           record, then use for setting keys
    C*================================================================
     C     chksff        begsr
B01  C                   if        *in04 = *on
     C     sfkey         chain     scr01s                             99
B02  C                   dow       *in99 = *off
     C                             and scdbrr = *zeros
     C                   eval      sfkey = sfkey + 1
     C     sfkey         chain     scr01s                             99
E02  C                   enddo
E01  C                   endif
B01  C                   if        *in99 = *off
     C                             and *in04 = *on
     C                   eval      *in08 = *on
     C                   update    scr01s
     C                   exsr      keysfl
X01  C                   else
     C                   exsr      keyinz
E01  C                   endif
     C                   endsr
    C*================================================================
    C*  CHKSFM - Check subfile mode for fold/truncate; if mode = '0'
    C*           then mode is fold, otherwise truncated
    C*================================================================
     C     chksfm        begsr
B01  C                   if        sflmod = '0'
     C                   move      *off          *in12
X01  C                   else
     C                   move      *on           *in12
E01  C                   endif
     C                   endsr
    C*================================================================
    C*  CHKVFK - Check for valid function key being pressed
    C*================================================================
     C     chkvfk        begsr
     C                   eval      of# = of# - of#
     C                   testb     '1'           q2iaid                   99
B01  C                   if        *in99 = *off
     C                   testb     '7'           q2iaid                   99
     C                   if        *in99 = *on
     C                   eval      of# = of# + 1
     C                   endif
     C                   testb     '6'           q2iaid                   99
     C                   if        *in99 = *on
     C                   eval      of# = of# + 2
     C                   endif
     C                   testb     '5'           q2iaid                   99
     C                   if        *in99 = *on
     C                   eval      of# = of# + 4
     C                   endif
     C                   testb     '4'           q2iaid                   99
     C                   if        *in99 = *on
     C                   eval      of# = of# + 8
     C                   endif
     C                   testb     '0'           q2iaid                   99
     C                   if        *in99 = *on
     C                   eval      of# = of# + 12
     C                   endif
B02  C                   if        vf(of#) = *off
     C                   eval      *in90 = *on
     C                   eval      *in94 = *on
E02  C                   endif
E01  C                   endif
     C                   endsr
    C*================================================================
    C*  CLRGET - Clears subfile; get new set of records
    C*================================================================
     C     clrget        begsr
     C                   exsr      clrsf1
     C                   exsr      getrds
     C                   endsr
    C*================================================================
    C*  CLRSF1 - Clear subfile; initializes relative record fields
    C*================================================================
     C     clrsf1        begsr
     C                   eval      lastrr = lastrr - lastrr
     C                   sub       sfrrn1        sfrrn1               04
     C                   eval      *in03 = *on
     C                   write     scr01c
     C                   eval      *in03 = *off
     C                   endsr
    C*================================================================
    C*  DLTEXE - Displays confirm delete subfile; processes records
    C*================================================================
     C     dltexe        begsr
     C                   eval      scrrn2 = 1
     C                   write     scr02
     C                   exfmt     scr02c
B01  C                   if        *inkl = *off
     C                   readc     scr02s                                 99
B02  C                   dow       *in99 = *off
     C                   move      'E'           ##mode
     C                   call      acdpgm        acdpl                  99
     C                   exsr      updsf1
     C                   readc     scr02s                                 99
E02  C                   enddo
E01  C                   endif
     C                   endsr
    C*================================================================
    C*  ENTRDO - Process the Enter key
    C*================================================================
     C     entrdo        begsr
B01  C                   if        scpst1 <> *blanks
     C                             or scpst2 <> *blanks
     C                             or scpst3 <> *blanks
     C                             or scpst4 <> *blanks
     C                             or scpst5 <> *blanks
     C                             or scpst6 <> *blanks
     C                             or scpst7 <> *blanks
     C                             or scpst8 <> *blanks
     C                             or scpst9 <> *blanks
     C                   exsr      keyset
     C                   exsr      clrget
     C                   eval      *in06 = *on
X01  C                   else
     C                   eval      scount = scount - scount
     C                   move      *off          sl1flg
B02  C                   if        *in04 = *on
B03  C                   if        q2lsfr <> *zeros
     C                   eval      scrrn1 = q2lsfr
E03  C                   endif
     C                   exsr      sf1chk
B03  C                   if        *in90 = *off
     C                   eval      *in06 = *on
     C                   exsr      sf1exe
B04  C                   if        *inkc = *off
     C                             and *inkl = *off
B05  C                   if        *in14 = *on
     C                   exsr      dltexe
E05  C                   endif
E04  C                   endif
     C                   sub       sfrrn2        sfrrn2               14
     C                   eval      *in13 = *on
     C                   write     scr02c
     C                   eval      *in13 = *off
E03  C                   endif
E02  C                   endif
B02  C                   if        *in90 = *off
B03  C                   if        scount = *zero
     C                             and vf(12) = *on
     C                             or scount > *zero
     C                             and sl1flg = *on
     C                   move      *on           outflg
E03  C                   endif
E02  C                   endif
B02  C                   if        *in90 = *off
     C                             and outflg = *off
     C                             and autorf = *on
     C                   exsr      kedo
     C                   move      *off          autorf
E02  C                   endif
E01  C                   endif
     C                   endsr
    C*================================================================
    C*  FNDRDB - Find a record (backwards)
    C*================================================================
     C     fndrdb        begsr
     C                   eval      @rcds = @rcds - @rcds
     C                   eval      @reads = @reads - @reads
     C                   eval      @readc = @readc - @readc
     C                   exsr      readp
B01  C                   dow       *in95 = *off
     C                             and @rcds < 1
     C                   eval      @reads = @reads + 1
     C                   eval      @readc = @readc + 1
     C                   move      *on           meetss
     C                   exsr      subset
B02  C                   if        meetss = *off
     C                   exsr      readck
     C                   if        *in95 = *off
     C                   exsr      readp
     C                   endif
     C                   else
     C                   eval      @rcds = @rcds + 1
E02  C                   endif
E01  C                   enddo
     C                   endsr
    C*================================================================
    C*  GETHD# - Get the heading line number; move heading to screen
    C*================================================================
     C     gethd#        begsr
     C                   eval      hd# = viewmx * info#
     C                   eval      hd# = hd# - viewmx
     C                   eval      hd# = hd# + view#
     C                   movel(p)  hd(hd#)       schdl1
     C                   eval      vi = view# + 20
     C                   movea     '00000000'    *in(21)
     C                   movea     '0'           *in(29)
     C                   move      *on           *in(vi)
     C                   endsr
    C*================================================================
    C*  GETCOF - Gets company and fund names if possible; otherwise
    C*           get PDS name for company name
    C*================================================================
     C     getcof        begsr
     C                   call      comapi        compl                  99
B01  C                   if        *in99 = *on
     C                   movel(p)  pdsnam        ##data
     C                   eval      ##conm = centerFld(##data)
E01  C                   endif
     C                   movel     ##conm        scconm
     C                   endsr
    C*================================================================
    C*  GETPGT - Get program titles for all screens
    C*================================================================
     C     getpgt        begsr
B01  C                   if        ##wwmo = '2'
     C     'Select'      cat(p)    optnm:1       ##data
X01  C                   else
     C     wwcons        cat(p)    optnm:1       ##data
E01  C                   endif
     C                   eval      scprds = centerFld(##data)
     C     cdcons        cat(p)    optnm:1       ##data
     C                   eval      scprd2 = centerFld(##data)
     C     sscons        cat(p)    optnm:1       ##data
     C                   eval      scprd3 = centerFld(##data)
     C                   endsr
    C*================================================================
    C*  GETRDB - Gets a page of records (backwards)
    C*================================================================
     C     getrdb        begsr
     C                   eval      @rcds = @rcds - @rcds
     C                   eval      @reads = @reads - @reads
     C                   eval      @readc = @readc - @readc
     C                   move      *off          firsti
B01  C                   dow       *in95 = *off
     C                             and @rcds <= sf1pag
     C                   eval      @reads = @reads + 1
     C                   eval      @readc = @readc + 1
     C                   move      *on           meetss
     C                   exsr      subset
B02  C                   if        meetss = *on
     C                   eval      @rcds = @rcds + 1
B03  C                   if        @rcds <= sf1pag
     C     sf1pag        sub       @rcds         sfrrn1               04
     C                   add       1             sfrrn1               04
B04  C                   if        firsti = *off
     C                   exsr      clrsf1
     C     sf1pag        sub       @rcds         sfrrn1               04
     C                   add       1             sfrrn1               04
     C                   move      *on           firsti
     C                   eval      lastrr = sfrrn1
E04  C                   endif
     C                   exsr      loadsf
E03  C                   endif
E02  C                   endif
B02  C                   if        @rcds <= sf1pag
     C                   exsr      readck
     C                   if        *in95 = *off
     C                   exsr      readp
     C                   endif
E02  C                   endif
E01  C                   enddo
     C                   eval      scrrn1 = sfrrn1
     C                   endsr
    C*================================================================
    C*  GETRDS - Gets a page of records
    C*================================================================
     C     getrds        begsr
B01  C                   if        sfrrn1 = *zeros
     C                   exsr      setll
B02  C                   if        *in96 = *off
     C                   exsr      read
B03  C                   if        *in95 = *on
     C                   exsr      setll
     C                   exsr      fndrdb
E03  C                   endif
X02  C                   else
     C                   exsr      fndrdb
E02  C                   endif
E01  C                   endif
     C                   eval      @rcds = @rcds - @rcds
     C                   eval      @reads = @reads - @reads
     C                   eval      @readc = @readc - @readc
     C                   move      *off          firsti
     C                   callp     sndPgmMsg('CPF9898': '*STATUS' : 'QCPFMSG' :
     C                             '*LIBL' : 'Searching...')
B01  C                   dow       *in95 = *off
     C                             and @rcds <= sf1pag
     C                   eval      @reads = @reads + 1
     C                   eval      @readc = @readc + 1
     C                   move      *on           meetss
     C                   exsr      subset
B02  C                   if        meetss = *on
     C                   eval      @rcds = @rcds + 1
B03  C                   if        @rcds <= sf1pag
     C                   add       1             sfrrn1               04
B04  C                   if        firsti = *off
     C                   move      *on           firsti
     C                   eval      scrrn1 = sfrrn1
E04  C                   endif
     C                   exsr      loadsf
E03  C                   endif
E02  C                   endif
B02  C                   if        sngitm = *off
B03  C                   if        @rcds <= sf1pag
     C                   exsr      readck
     C                   if        *in95 = *off
     C                   exsr      read
     C                   endif
E03  C                   endif
X02  C                   else
     C                   eval      *in95 = *on
E02  C                   endif
E01  C                   enddo
     C                   eval      lastrr = sfrrn1
     C                   endsr
    C*================================================================
    C*  KCDO   - Process F3; end of program
    C*================================================================
     C     kcdo          begsr
B01  C                   if        ##wwmo = ' '
     C                             or ##wwmo = '1'
B02  C                   if        q1prms <= 3
     C                   callp     runCmd(rclcmd : 14)
     C                   move      *on           *inlr
E02  C                   endif
E01  C                   endif
     C                   endsr
    C*================================================================
    C*  KEDO   - Process F5; refreshes subfile
    C*================================================================
     C     kedo          begsr
     C*** BEGIN ADD ****** 02/06/12 ************************************
      /IF DEFINED(F5_entry)
     C                   exsr      enKedo
      /ENDIF
     C*** END   ADD ****** 02/06/12 ************************************
     C                   eval      sfkey = 1
     C                   exsr      chksff
     C                   exsr      clrget
     C                   callp     rmvMsg
     C                   exsr      sndssm
     C*** BEGIN ADD ****** 02/06/12 ************************************
      /IF DEFINED(F5_exit)
     C                   exsr      exKedo
      /ENDIF
     C*** END   ADD ****** 02/06/12 ************************************
     C                   endsr
    C*================================================================
    C*  KFDO   - Process F6; add records
    C*================================================================
     C     kfdo          begsr
     C                   clear                   scdbrr
     C                   move      'A'           ##mode
     C                   call      acdpgm        acdpl                  99
     C                   if        *inkc = *off
     C                   exsr      kedo
     C                   endif
     C                   endsr
    C*================================================================
    C*  KIDO   - Process F9; change info
    C*================================================================
     C     kido          begsr
     C                   eval      info# = info# + 1
B01  C                   if        info# > infomx
     C                             or info# = *zero
     C                   eval      info# = 1
E01  C                   endif
     C                   exsr      gethd#
     C                   eval      sfkey = q2lsfr
     C                   exsr      chksff
     C                   exsr      clrget
     C                   endsr
    C*================================================================
    C*  KKDO   - Process F11; change view
    C*================================================================
     C     kkdo          begsr
     C                   eval      view# = view# + 1
B01  C                   if        view# > viewmx
     C                             or view# = *zero
     C                   eval      view# = 1
E01  C                   endif
     C                   exsr      gethd#
     C                   eval      *in06 = *off
     C                   exsr      keyinz
     C                   exsr      clrget
     C                   endsr
    C*================================================================
    C*  KLDO   - Process F12; cancel program
    C*================================================================
     C     kldo          begsr
     C                   move      *on           outflg
     C                   endsr
    C*================================================================
    C*  KMDO   - Process F13; user parameters
    C*================================================================
     C     kmdo          begsr
     C                   call      uspapi                               99
     C                   exsr      getcof
     C                   exsr      keyinz
     C                   exsr      pgmstr
     C                   exsr      clrget
     C                   endsr
    C*================================================================
    C*  KRDO   - Process F17; subset selection
    C*================================================================
     C     krdo          begsr
B01  C                   dou       *in90 = *off
     C                   exfmt     scr03
     C                   movea     '00000'       *in(71)
     C                   movea     '00000'       *in(76)
     C                   movea     '00000'       *in(81)
     C                   movea     '000000'      *in(86)
B02  C                   if        *inkl = *off
     C                   exsr      errck2
B03  C                   if        *in90 = *off
     C                   callp     rmvMsg
     C                   exsr      sndssm
     C                   exsr      keyinz
     C                   exsr      clrget
E03  C                   endif
E02  C                   endif
E01  C                   enddo
     C                   endsr
    C*================================================================
    C*  KXDO   - Process F23; more options
    C*================================================================
     C     kxdo          begsr
     C                   eval      ol#1 = ol#2 + 1
B01  C                   if        ol#1 > 12
     C                             or ol(ol#1) = *blanks
     C                   eval      ol#1 = 1
E01  C                   endif
     C                   eval      ol#2 = ol#1 + 1
     C                   movel     ol(ol#1)      scopl1
     C                   movel     ol(ol#2)      scopl2
B03  C                   if        q2lsfr <> *zeros
     C                   eval      scrrn1 = q2lsfr
E03  C                   endif
     C                   endsr
    C*================================================================
    C*  KYDO   - Process F24; more keys
    C*================================================================
     C     kydo          begsr
     C                   eval      fl#1 = fl#2 + 1
B01  C                   if        fl#1 > 12
     C                             or fl(fl#1) = *blanks
     C                   eval      fl#1 = 1
E01  C                   endif
     C                   eval      fl#2 = fl#1 + 1
     C                   movel     fl(fl#1)      scfkl1
     C                   movel     fl(fl#2)      scfkl2
B03  C                   if        q2lsfr <> *zeros
     C                   eval      scrrn1 = q2lsfr
E03  C                   endif
     C                   endsr
    C*================================================================
    C*  LOADSF - Load item into subfile
    C*================================================================
     C     loadsf        begsr
     C                   eval      *in05 = *off
     C                   eval      *in07 = *off
     C                   eval      *in08 = *off
B01  C                   select
S01  C                   when      view# = 1
     C                   eval      scdbrr = dbrrn
S02  C                   when      view# = 2
     C                   eval      scdbrr = dbrrn2
S03  C                   when      view# = 3
     C                   eval      scdbrr = dbrrn3
S04  C                   when      view# = 4
     C                   eval      scdbrr = dbrrn4
S05  C                   when      view# = 5
     C                   eval      scdbrr = dbrrn5
S06  C                   when      view# = 6
     C                   eval      scdbrr = dbrrn6
S07  C                   when      view# = 7
     C                   eval      scdbrr = dbrrn7
S08  C                   when      view# = 8
     C                   eval      scdbrr = dbrrn8
S09  C                   when      view# = 9
     C                   eval      scdbrr = dbrrn9
E01  C                   endsl
     C                   clear                   scsel
     C                   exsr      mvdbsc
     C                   write     scr01s
     C                   endsr
    C*================================================================
    C*  OPT1   - Process option 1; select item
    C*================================================================
     C     opt1          begsr
     C                   move      *on           sl1flg
     C                   exsr      slctrd
     C                   endsr
    C*================================================================
    C*  OPT2   - Process option 2; change item
    C*================================================================
     C     opt2          begsr
     C                   move      'C'           ##mode
     C                   call      acdpgm        acdpl                  99
     C                   endsr
    C*================================================================
    C*  OPT3   - Process option 3; copy item
    C*================================================================
     C     opt3          begsr
     C                   move      'P'           ##mode
     C                   call      acdpgm        acdpl                  99
     C                   endsr
    C*================================================================
    C*  OPT4   - Process option 4; delete item
    C*================================================================
     C     opt4          begsr
     C                   eval      scsf1r = sfrrn1
     C                   add       1             sfrrn2               14
     C                   write     scr02s
     C                   endsr
    C*================================================================
    C*  OPT5   - Process option 5; display item
    C*================================================================
     C     opt5          begsr
     C                   move      'D'           ##mode
     C                   call      acdpgm        acdpl                  99
     C                   endsr
    C*================================================================
    C*  PGDNDO - Process page down; get another page in subfile
    C*================================================================
     C     pgdndo        begsr
B01  C                   if        *in95 = *off
     C                   eval      sfkey = lastrr
     C                   exsr      chksfb
     C                   eval      sfrrn1 = lastrr
     C                   exsr      setgt
B02  C                   if        *in96 = *on
     C                   eval      *in90 = *on
     C                   eval      *in91 = *on
E02  C                   endif
     C                   exsr      read
B02  C                   if        *in95 = *on
     C                   eval      *in90 = *on
     C                   eval      *in91 = *on
     C                   eval      *in95 = *off
X02  C                   else
     C                   exsr      getrds
E02  C                   endif
X01  C                   else
     C                   eval      *in90 = *on
     C                   eval      *in91 = *on
E01  C                   endif
     C                   endsr
    C*================================================================
    C*  PGUPDO - Process page up; get page for subfile
    C*================================================================
     C     pgupdo        begsr
     C                   move      *in95         sv95
     C                   eval      sfkey = 1
     C                   exsr      chksff
B01  C                   if        sngitm = *off
     C                   exsr      setll
     C                   exsr      readp
E01  C                   endif
B01  C                   if        *in95 = *on
     C                   eval      *in90 = *on
     C                   eval      *in91 = *on
     C   04              readc     scr01s                                 99
B02  C                   if        sngitm = *off
     C                   move      sv95          *in95
E02  C                   endif
X01  C                   else
     C                   exsr      getrdb
B02  C                   select
S01  C                   when      @rcds = 0
     C                   eval      *in90 = *on
     C                   eval      *in91 = *on
B03  C                   if        lastrr >= sf1pag
     C                   if        *in95 = *on
     C                   eval      *in95 = *off
     C                   endif
E03  C                   endif
S02  C                   when      @rcds > sf1pag
     C                   if        *in95 = *on
     C                   eval      *in95 = *off
     C                   endif
S03  C                   other
     C                   exsr      keyinz
     C                   exsr      clrget
E02  C                   endsl
E01  C                   endif
     C                   endsr
    C*================================================================
    C*  READCK - Checks the number of reads; calls SRRDCCKR if necess
    C*================================================================
     C     readck        begsr
     C     @readc        div       @incc         @readr
     C                   mvr                     @readr
B01  C                   if        @readr = 0
     C                   eval      @cont = readCheck(@reads : @rcds)
B02  C                   if        @cont = '1'
     C                   eval      *in95 = *on
E02  C                   endif
B02  C                   if        @cont = '2'
     C                   eval      @readc = @readc - @readc
E02  C                   endif
B02  C                   if        @cont = '3'
     C                   eval      @readc = @readc - @readc
     C                   eval      @incc = @inc * 3
E02  C                   endif
B02  C                   if        @cont = '4'
     C                   eval      @readc = @readc - @readc
     C                   eval      @incc = @inc * 5
E02  C                   endif
B02  C                   if        @cont = '5'
     C                   eval      @readc = @readc - @readc
     C                   movel     *hival        @incc
E02  C                   endif
E01  C                   endif
     C                   endsr
    C*================================================================
    C*  REFRSR - Refreshes the item in the subfile from data base
    C*================================================================
     C     refrsr        begsr
B01  C                   if        *inkc = *off
     C     scdbrr        chain     pfrcdf                             99
B02  C                   if        *in99 = *off
     C                   exsr      mvdbsc
E02  C                   endif
E01  C                   endif
     C                   endsr
    C*================================================================
    C*  SF1CHK - Read subfile changes; checks for errors
    C*================================================================
     C     sf1chk        begsr
     C                   eval      count1 = count1 - count1
     C                   movea     '00000'       *in(71)
     C                   movea     '00000'       *in(76)
     C                   movea     '00000'       *in(81)
     C                   movea     '0000'        *in(86)
     C                   movea     '00'          *in(92)
     C                   readc     scr01s                                 99
B01  C                   dow       *in99 = *off
     C                   eval      *in05 = *off
     C                   eval      *in07 = *off
     C                   eval      *in08 = *off
B02  C                   if        scsel <> *blanks
B03  C                   if        *in90 = *off
     C                   exsr      chkerr
B04  C                   if        *in90 = *off
     C                   exsr      errck1
B05  C                   if        *in90 = *off
     C                             and scsel = '1'
     C                   eval      count1 = count1 + 1
E05  C                   endif
E04  C                   endif
B04  C                   if        count1 > 1
     C                   eval      *in90 = *on
     C                   eval      *in92 = *on
E04  C                   endif
B04  C                   if        *in90 = *on
     C                   eval      *in05 = *on
     C                   eval      *in07 = *on
     C                   eval      scrrn1 = sfrrn1
E04  C                   endif
E03  C                   endif
     C                   eval      *in08 = *on
E02  C                   endif
     C                   update    scr01s
     C                   eval      *in05 = *off
     C                   eval      *in07 = *off
     C                   eval      *in08 = *off
     C                   readc     scr01s                                 99
E01  C                   enddo
     C                   endsr
    C*================================================================
    C*  SF1EXE - Process subfile changes
    C*================================================================
     C     sf1exe        begsr
     C                   readc     scr01s                                 99
B01  C                   dow       *in99 = *off
     C                   eval      *in07 = *off
     C                   eval      *in08 = *off
B02  C                   if        scsel <> *blanks
     C                   eval      scount = scount + 1
     C     scsel         caseq     '1'           opt1
     C     scsel         caseq     '2'           opt2
     C     scsel         caseq     '3'           opt3
     C     scsel         caseq     '4'           opt4
     C     scsel         caseq     '5'           opt5
     C                   cas                     othopt
     C                   endcs
     C                   eval      *in08 = *on
B03  C                   if        scsel <> '4'
     C                   clear                   scsel
     C                   eval      scrrn1 = sfrrn1
     C                   exsr      refrsr
E03  C                   endif
E02  C                   endif
     C                   update    scr01s
     C                   if        *inkc = *off
     C                             and *inkl = *off
     C                   readc     scr01s                                 99
     C                   endif
     C  nkc
E01  Cannkl              enddo
     C                   endsr
    C*================================================================
    C*  SNDSSM - Send subsetted list message if necessary
    C*================================================================
     C     sndssm        begsr
     C                   move      *off          *in11
     C                   exsr      chkssm
B01  C                   if        *in11 = *on
     C                   callp     sndPgmMsg(slmsgi : '*INFO' : slmsgf :
     C                             '*LIBL' )
E01  C                   endif
     C                   endsr
    C*===============================================================
    C* UPDOFA - Update values in the option/function array
    C*===============================================================
    C*  Before calling this subroutine make the following assignments:
    C*       XXOPFN = '1' if Option, '2' if Function
    C*       XXNMBR = Option/Function number to change
    C*            NOTE: Pass option/function numbers as 2 char fields.
    C*                  e.g. MOVE '05' XXNMBR or MOVE '20' XXNMBR
    C*       XXMIS = 'XXX' where each 'X' is Y or N depending on change rqrd
    C*
    C*  This subroutine when called evaluates array OF for XXOPFN and checks
    C*  OFD to see if it is the option/function that matches XXNMBR. If so, it
    C*  replaces 'XXX' with XXMIS.
    C*
     C     updofa        begsr
     C                   move      *off          found
     C                   eval      i = 1
    C*
    C*  Go through array looking for a match
    C*
B01  C                   dow       found = *off
     C                             and of(i) <> '3'
    C*
B02  C                   if        of(i) = xxopfn
     C                   movel     ofd(i)        dumnum
B03  C                   if        xxnmbr = dumnum
     C                   movel(p)  ofd(i)        dumofd
     C                   move      xxmis         dumofd
     C                   movel     dumofd        ofd(i)
     C                   move      *on           found
E03  C                   endif
E02  C                   endif
    C*
     C                   eval      i = i + 1
E01  C                   enddo
     C                   endsr
    C*================================================================
    C*  UPDSF1 - Updates subfile record when item is deleted
    C*================================================================
     C     updsf1        begsr
     C     scsf1r        chain     scr01s                             99
B01  C                   if        *in99 = *off
     C                   clear                   scr01s
     C                   exsr      sf1upd
E01  C                   endif
     C                   update    scr01s
     C                   endsr
