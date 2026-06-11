     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller)
     H/define F5_entry
     H/define F5_exit
     H*********************************************************************
     H/title IS215R - Work with User Queued Entries                       *
     H*********************************************************************
     H*  PROGRAM DESCRIPTION:                                             *
     H*    This program displays a list of user queued entries            *
     H*                                                                   *
     H*********************************************************************
     H* Revisions:                                                        *
     H*                                                                   *
     H* 07/10/19 C:062 Project: 152WAB                                    *
     H*                Don't close FAAUP unless LR.                       *
     H*                                                                   *
     H* 04/10/19 C:062 Project: 152WAB                                    *
     H*                Apply CSI mods for S1 Medical.                     *
     H*                                                                   *
     H* 10/05/16  C:062 Support: 50062(150)                               *
     H*                 Managed Care file needs to be usropn              *
     H*                                                                   *
     H* 10/22/14  C:302 Support: 84408                                    *
     H*                - Reworked call to IS216R so IS216R always         *
     H*                  writes the notes after a delete.                 *
     H*                                                                   *
     H* 10/20/14  C:302 Support: 87069                                    *
     H*                - Refresh document list after F12.                 *
     H*                                                                   *
     H* 06/11/14  C:302 Support: 84664                                    *
     H*                - Fix F12 Behavior.  Clean up program              *
     H*********************************************************************
     Fis215f    cf   e             workstn infds(fids01)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     sfile(scr04s:sfrrn4)

     F*  Physical file definition (for refreshing subfile record)
     FIXIEP     if   e             disk    rename(pie01:pfrcdf)
     F*  Primary logical file definition (for displaying items)
     FIXIELE    if   e           k disk    infds(dbfds)
     F                                     rename(pie01:dbrcdf)
     FIXIELF    if   e           k disk    infds(dbfds2)
     F                                     rename(pie01:dbrcdf2)
     FIXIELG    if   e           k disk    infds(dbfds3)
     F                                     rename(pie01:dbrcdf3)
     FIXIELP    if   e           k disk    infds(dbfds4)
     F                                     rename(pie01:dbrcdf4)
     FIXIELN    if   e           k disk    infds(dbfds5)
     F                                     rename(pie01:dbrcdf5)
     FIXIELK    if   e           k disk    infds(dbfds6)
     F                                     rename(pie01:dbrcdf6)
     F*** Begin Chg  *** 10/05/17 **********************************
     F***FAAUL1    if   e           k disk
     FFAAUL1    if   e           k disk    UsrOpn
     F*** End   Chg  *** 10/05/17 **********************************
     FIMIELF    uf   e           k disk    rename(pie01:pie01f)
     FIMDOL1    if   e           k disk
     FIMDOLD    if   e           k disk    rename(pdo01:pdo0d)
     FIMDTL     if   e           k disk    rename(pdt01:pdt00)
     FIMIFL2    if   e           k disk    rename(pif01:pif02)
     FIMDNL3    if   e           k disk    rename(pdn01:pdn03)
     FSDCMLE    if   e           k disk    rename(pcm01:pcm0e)
     FSSYLF     if   e           k disk
     D*
     D#ofHD            c                   12
     D hd              s             78    dim(#ofHD) ctdata perrcd(1)
     D#ofOFD           c                   17
     D of              s              1    dim(#ofOFD) ctdata perrcd(1)
     D ofd             s             35    dim(#ofOFD) alt(of)
     D#ofDN            c                   5
     D dn              s             75    dim(#ofDN)
     DsvInd            s              1n   dim(99)
     D optnm           c                   'User Queued Index Entries'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'SYMESG'
     D acdpgm          c                   'IS215R02'
     D comapi          c                   'SA005R'
     D uspapi          c                   'S0006R'
     D infoMx          c                   2
     D viewMx          s              1  0 inz(1)
     D actualViewMx    c                   6
     D sf1pag          c                   12

     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D @@QID           s             10
     D @@RtCd          s              1
     D @#Call          s              1
     D @@Call          s              1
     D cltID           s              3
     D kkQID           s                   like(IEQID)
     D kkSDt           s                   like(IESDT)
     D kkSTm           s                   like(IESTM)
     D kkDcID          s                   like(IEDCID)
     D kkDocT          s                   like(IEDOCT)
     D kkRcvD          s                   like(IERCVD)
     D sfrrn4          s              4  0
     D ##QID           s                   like(@@QID)
     D ##SDt           s                   like(scSDt)
     D ##STm           s                   like(scSTm)
     D ##DcId          s                   like(scDcId)
     D ##DocT          s                   like(scDocT)
     D ##Desc          s             30
     D ##Flr           s                   like(scFlr)
     D ##SFlr          s                   like(scSFlr)
     D ##Pth           s                   like(scPth)
     D ##Fl01          s                   like(scFl01)
     D ##FileName      s             10
     D ##LibName       s             10
     D ##MbrName       s             10
     D ##RtCd          s              1
     D #6RtCd          s              6
     D xxData2         s                   like(scData2)
     D x1Data2         s                   like(scData2)
     D x2Data2         s                   like(scData2)
     D xxedt           s              8  0
     D svRtCd          s                   like(##RtCd)
     D svRRN1          s                   like(sfRRN1)
     D opt2Selected    s              1n
     D opt2Single      s              1n
     D count2          s              5  0
     D firstReassign   s              1n
     D error           s              1n
     D i1              s              3  0
     D fill40          s             40    inz

     Dsca              ds
     D SC10A0                        10
     D SC10A1                        10
     D SC10A2                        10
     D SC10A3                        10
     D SC10A4                        10
     D SC10A5                        10
     D SC10A6                        10
     D SC10A7                        10
     D SC10A8                        10
     D SC10A9                        10
     Ddta              ds
     D DT10A0                        10
     D DT10A1                        10
     D DT10A2                        10
     D DT10A3                        10
     D DT10A4                        10
     D DT10A5                        10
     D DT10A6                        10
     D DT10A7                        10
     D DT10A8                        10
     D DT10A9                        10
     d xxindk          ds
     d xxco#                   1      3
     d xxfnd                   4      6
     d xxfyr                   7      9
     d xxcase                 10     16

     DAlphaTime        pr             8
     D inTime                         6  0 const

     DCvtSDtMDYY       pr             8  0
     D inDate                         8  0 const

     DCvtSDtYYMD       pr             8  0
     D inDate                         8  0 const

     DDspFax           pr                  extPgm('DSPFAXC')
     D ##DcId                        12    const
     D ##Fold                        80    const
     D ##RtCd                         1

     DDspImg           pr                  extPgm('IS211C')
     D ##DcId                        12    const
     D ##Flr                          8    const
     D ##SFlr                        12    const
     D ##DocT                        10    const
     D ##Desc                        40    const
     D ##RtCd                         1

     DIndexRcd         pr                  extPgm('IS215R1')
     D ##QID                         10    const
     D ##DcId                        12    const
     D ##SDt                          8    const
     D ##STm                          6    const
     D ##DocT                         8    const
     D ##RtCd                         1

     DNotes            pr                  extPgm('IS016R')
     D ##DcId                        12    const
     D ##PgmName                     10    const

     DPrintFax         pr                  extPgm('ISFAXP')
     D ##DcId                        12    const
     D ##Fold                        80    const
     D ##Key                         40    const
     D ##Seq#                         5    const
     D ##Fl05                         1    const
     D ##RtCd                         6
     D ##QID                         10    const
     D ##UncN                       255    const options(*nopass)
     D ##IfsP                         1    const options(*nopass)

     DPrintImg         pr                  extPgm('ISIMGP')
     D ##DcDd                        12    const
     D ##Fold                        80    const
     D ##Key                         40    const
     D ##Seq#                         5    const
     D ##RtCd                         6
     D ##Fl05                         1    const
     D ##QID                         10    const

     DReassign         pr                  extPgm('IS115R')
     D ##SDt                          8    const
     D ##STm                          6    const
     D ##DocT                         8    const
     D ##DcId                        12    const
     D ##Fl01                         1    const
     D ##Flr                          8    const
     D ##SFlr                        12    const
     D ##Pth                         63    const
     D ##QID                         10    const
     D ##Desc                        30    const
     D ##RtCd                         1

     DRemove           pr                  extPgm('IS216R')
     D ##SDt                          8    const
     D ##STm                          6    const
     D ##DcId                        12    const
     D ##QID                         10    const
     D qqPgm                         10    const
     D qqSilent                       1    const
     D ##RtCd                         1

     DRtvCltID         pr                  extPgm('RTVCLTID')
     D cltID                          3    const

     DSplit            pr                  extPgm('IS215R3')
     D ##QID                         10    const
     D ##DcId                        12    const
     D ##SDt                          8    const
     D ##STm                          6    const
     D ##DocT                         8    const
     D ##RtCd                         1

     DWWAllQs          pr                  extPgm('IS120R2')
     D ##QID                         10
     D ##Desc                        40    const
     D ##RtCd                         1

     DWWSharedQs       pr                  extPgm('IS120R1')
     D ##QID                         10
     D ##Desc                        40    const
     D ##RtCd                         1

     DCvtSDt           pr             8  0
     D inDt                           8  0

     DIndexCd          pr             1
     D inFlag                         1

     C/COPY SCOPYWW42C

     C     *entry        plist
     C                   parm                    @@QID
     C                   parm                    @@RtCd
     C                   parm                    @#Call

     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn

     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##conm
     C     a0pl          plist

     C                   parm      scQID         ##QID
     C                   parm                    q1User
     C                   parm      scSDt         ##SDt
     C                   parm      scSTm         ##STm
     C                   parm      scDcId        ##DcId
     C                   parm      scDesc        ##Desc
     C                   parm      scDocT        ##DocT
     C                   parm      scFl01        ##Fl01
     C                   parm      scPth         ##Pth
     C                   parm      scFlr         ##Flr
     C                   parm      scSFlr        ##SFlr
     C                   parm                    ##RtCd

     C     key           klist
     C                   kfld                    kkQID
     C     key1          klist
     C                   kfld                    kkQID
     C                   kfld                    kkSDt
     C                   kfld                    kkSTm
     C     key2          klist
     C                   kfld                    kkQID
     C                   kfld                    kkDcId
     C                   kfld                    kkSDt
     C                   kfld                    kkSTm
     C     key3          klist
     C                   kfld                    kkQID
     C                   kfld                    kkDocT
     C                   kfld                    kkSDt
     C                   kfld                    kkSTm
     C     key4          klist
     C                   kfld                    kkQID
     C                   kfld                    kkRcvD
     C                   kfld                    kkSDt
     C                   kfld                    kkSTm
      ** Based on key list of logical file IXIELN based on file IXIEP.
     c     key5          klist
     c                   kfld                    kkQID
     c                   kfld                    kkEntN           60
     c                   kfld                    kkSDt
     c                   kfld                    kkSTm
     c                   kfld                    kkdseq            5 0
      ** Based on key list of logical file IXIELK based on file IXIEP.
     c     key6          klist
     c                   kfld                    kkQID
     c                   kfld                    kkIndK           40
     c                   kfld                    kkSDt
     c                   kfld                    kkSTm
     c                   kfld                    kkdseq
     C     keyIMDNL3     klist
     C                   kfld                    IEDCID
      ** Key list based in file FAAUL1
     c     kyau1         klist
     c                   kfld                    xxco#1
     c                   kfld                    #sub#1
     c                   kfld                    #mcf#1
     c                   kfld                    xxfyr1
     c                   kfld                    xxcase1
     c                   kfld                    xxdcid
      ** Key list based in file IMIELF
     c     kyie1         klist
     c                   kfld                    scqid
     c                   kfld                    scdcid
     c                   kfld                    scsdt
     c                   kfld                    scstm
      *====================================================================
      * ChkSsM - Set indicator 11 for subset message
      *====================================================================
     C     ChkSsM        begsr
     C                   endsr
      *
      *====================================================================
      * ErrCk1 - Additional errors on subfile selection
      *====================================================================
     C     ErrCk1        begsr
      *
     C                   eval      scOpt = scSel
      *
     C                   select
     C                   when      scSel <> '2' and opt2Selected
     C                   seton                                        71  90
     C                   when      scSel = '2'
     C                   eval      scSel = '#2'
     C                   eval      opt2Selected = *on
     C                   eval      count2 += 1
     C                   eval      opt2Single = count2 = 1
     C                   when      scSel = '4'
     C                   eval      scSel = '#4'
     C                   when      scSel = '5'
     C                   eval      scSel = '#5'
     C                   endsl
      *
     C                   if        *in90
     C                   eval      opt2Selected = *off
     C                   eval      count2 = 0
     C                   exsr      ResetSel
     C                   endif
      *
     C                   endsr
      *
      *====================================================================
      * ErrCk2 - Error checking for the subset screen
      *====================================================================
     C     ErrCk2        begsr
     C                   endsr
      *
      *====================================================================
      * GetActualHd# - Get the actual header # based on actualViewMx vs viewMx
      *====================================================================
     C     GetActualHd#  begsr
      *
     C                   eval      hd# = actualViewMx * info#
     C                                 - viewmx + view#
     C                   eval      scHdL1 = hd(hd#)
      *
     C                   endsr
      *
      *====================================================================
      * HelpDo - Process help request (Probably not needed)
      *====================================================================
     C     HelpDo        begsr
      *
     C                   call      'M0006C'
     C                   parm      'QHLP      '  ##FileName
     C                   parm      '*LIBL     '  ##LibName
     C                   parm      'IS215RCR01'  ##MbrName
      *
     C                   endsr
      *
      *====================================================================
      * InzSst - Initialize the subset fields
      *====================================================================
     C     InzSst        begsr
      *
     C                   exsr      GetActualHd#
      *
     C                   endsr
      *
      *====================================================================
      * KgDo - Process F7
      *====================================================================
     C     KgDo          begsr
     C*
     C     key           setll     dbRcdF
     C                   dou       %eof
     C     key           reade     dbRcdF
     C                   if        not %eof
     C                   eval      ##RtCd = IndexCd(IEFL01)
     C                   callp     IndexRcd(IEQID
     C                                     :IEDCID
     C                                     :%editc(CvtSDtMDYY(IESDT):'Z')
     C                                     :%editc(IESTM:'X')
     C                                     :IEDOCT
     C                                     :##RtCd
     C                                     )

      /free

       Select;
       when ##RtCd = '3'; // F3;
         exsr PgmEnd;
         return;
       when ##RtCd = 'L'; // Locked;
         iter;
       when ##RtCd = 'C'; // F12;
         leave;
       when ##RtCd = 'P'; // Bypassed;
         iter;
       endsl;

      /end-free

     C                   endif
     C                   enddo
      *
     C                   exsr      KeDo
      *
     C                   endsr
      *
      *====================================================================
      * KeyInz - Initialize key values for program start
      *====================================================================
     C     KeyInz        begsr
     C                   exsr      GetActualHd#
      *
     C                   clear                   kkSDt
     C                   clear                   kkSTm
     C                   clear                   kkDcId
     C                   clear                   kkDocT
     C                   clear                   kkRcvD
     c                   clear                   kkEntN
     c                   clear                   kkIndK
     c                   clear                   kkDSeq
      *
     C                   endsr
      *
      *====================================================================
      * KeySet - Set key values when doing "position to"
      *====================================================================
     C     KeySet        begsr
      *
     C                   exsr      GetActualHd#
      *
     C                   clear                   kkSDt
     C                   clear                   kkSTm
      *
     C                   select
     C                   when      view# = 1
     C                   eval      kkSDt = CvtSDtYYMD(numDate(scPst1))
     C                   Monitor
     C                   eval      kkSTm = %dec(%Trim(scPst1A):6:0)
     C                   On-Error
     C                   eval      kkSTm = 0
     C                   EndMon
     C                   when      view# = 2
     C                   eval      kkDcId = scPst2
     C                   when      view# = 3
     C                   eval      kkDocT = scPst3
     C                   when      view# = 4
     C                   eval      kkRcvD = CvtSDtYYMD(numDate(scPst4))
     C                   when      view# = 5
     C                   eval      kkEntN = scPst5
     C                   when      view# = 6
     C                   eval      kkIndK = scPst6
     C                   endsl
      *
     C                   endsr
      *
      *====================================================================
      * KeySfl - Set key values from subfile records for paging
      *====================================================================
     C     KeySfl        begsr
      *
     C                   exsr      GetActualHd#
      *
     C                   eval      kkSDt  = scSDt
     C                   eval      kkSTm  = scSTm
     C                   eval      kkDcId = scDcId
     C                   eval      kkDocT = scDocT
     C                   eval      kkRcvD = scRcvD
     C                   eval      kkEntN = scEntN
     C                   eval      kkIndK = scIndK
     C                   eval      kkDSeq = scDSeq
      *
     C                   endsr
      *
      *====================================================================
      * MvDbSc - Move data base fields to screen fields
      *====================================================================
     C     MvDbSc        begsr
      *
      *
     C                   eval      hd# = actualViewMx * info#
     C                                 - actualViewMx + view#
     C                   eval      scHdL1 = hd(hd#)
      *
     C                   eval      opt2Selected = *off
     C                   eval      count2 = 0
      *
     C                   if        *in90
     C                   eval      scSel = scOpt
     C                   endif
      *
     C                   eval      scQID  = IEQID
     C                   eval      scSDt  = IESDT
     C                   eval      scSTm  = IESTM
     C                   eval      scDcId = IEDCID
     C                   eval      scDocT = IEDOCT
     C                   eval      scRcvD = IERCVD
     C                   eval      scEntN = IENAME
     C                   eval      scIndK = IEDKEY
     C                   eval      scdseq = IEDSQ#
     C                   eval      scFl01 = IEFL01
     C                   eval      scFl02 = IEFL02
     C                   eval      scFl05 = IEFL05
      *
     C                   clear                   DOFLR
     C                   clear                   DOSFLR
     C                   clear                   DOUNCN
     C                   clear                   DOIFSP
     C     IEDCID        chain     pdo01
     C                   eval      scFlr  = DOFLR
     C                   eval      scSFlr = DOSFLR
     C                   eval      scPth  = %trim(DOFLR) + '/' + %trim(DOSFLR)
     C                   eval      scUncN = DOUNCN
     C                   eval      scIfsP = DOIFSP
      *
     C                   clear                   dta
     C                   eval      DTDESC = '*N/A*'
     C     IEDOCT        chain     pdt00
     C                   eval      sca = dta
     C                   eval      x1Data2 = DTDESC
     C                   eval      scDesc  = DTDESC
      *
     C                   select
     C                   when      IEFL01 = 'F'
     C                   eval      x1Data2 = '*N/A*'
     C                   clear                   IFTSI
     C     IEDCID        chain     pif02
     C                   if        IFTSI <> *blank
     C                   eval      x1Data2 = 'Fax From: ' + IFTSI
     C                   endif
     C                   endsl
      *
     C                   clear                   DODKEY
     C                   clear                   DODSQ#
     C                   clear                   CMCOMM
     C     IEDCID        chain     pdo0d
     c                   eval      x2Data2 = iecomm
     c                   eval      scComm  = iecomm
      *
     C                   eval      *in16 = *off
     C                   select
     C                   when      scFl02 = 'Y'
     C                   eval      *in16 = *on
     C                   endsl
      *
     C                   eval      *in33 = *off
     C                   clear                   scData2
     c                   clear                   scData1
      *
     c                   if        view# = 5
     c                   movel     iedkey        xxdkey1          20
     c                   eval      scdata1 = xxdkey1 + ' ' + iedoct + ' ' +
     c                             AlphaUSA(CvtToDate(CvtSDtMDYY(IESDT)))
     c                   endif
      *
     c                   if        view# = 6
     c                   movel     iename        xxname1          20
     c                   eval      scdata1 = xxname1 + ' ' + iedoct + ' ' +
     c                             AlphaUSA(CvtToDate(CvtSDtMDYY(IESDT)))
     c                   endif
     C                   select
     C                   when      xxData2 <> *blank
     C                   eval      scData2 = xxData2
     C                   eval      *in33 = *on
     C                   clear                   xxData2
     C                   when      info# = 2
     C                   eval      scData2 = x2Data2
     C                   other
     C                   eval      scData2 = x1Data2
     C                   endsl
      *
     c                   movel     iename        xxname           20
     c                   movel     iedkey        xxdkey           20
     c                   movel     iedcid        xxdocid           8
      *
      * Converting the entity date into MMDDYY format.
     c                   eval      xxedt = dtmdyy(ieedt)
     c
     c
      /free

       Select;
       when view# = 1;
         scData = AlphaUSA(CvtToDate(CvtSDtMDYY(IESDT))) + ' '
                + AlphaTime(IESTM) + ' '
                + IEDCID + ' '
                + IEDOCT + ' '
                ;
       when view# = 2;
         scData = IEDCID + ' '
                + AlphaUSA(CvtToDate(CvtSDtMDYY(IESDT))) + ' '
                + AlphaTime(IESTM) + ' '
                + IEDOCT
                ;
       when view# = 3;
         scData = IEDOCT + ' ' + IEDCID + ' '
                + AlphaUSA(CvtToDate(CvtSDtMDYY(IESDT))) + ' '
                + AlphaTime(IESTM) + ' '
                ;
       when view# = 4;
         scData = AlphaUSA(CvtToDate(CvtSDtMDYY(IERCVD))) + '  '
                + IEDCID + '  '
                + IEDOCT + ' '
                ;
       //  Adding the view 5 and view 6
       when view# = 5;
         scData = xxname + ' ' + AlphaUSA(CvtToDate(CvtSDtMDYY(xxedt))) + ' '
                + xxdocid + ' '
                ;
       when view# = 6;
         scData = xxdkey + ' ' + AlphaUSA(CvtToDate(CvtSDtMDYY(xxedt))) + ' '
                + xxdocid + ' '
                ;
       endsl;

      /end-free
      *
     C                   select
     C*** Begin Chg *** 04/10/19 ************************************
     C**********         when      cltID = '114'
     C                   when      cltID = '114' Or cltID = '152'
     C*** End   Chg *** 04/10/19 ************************************
     C                   exsr      MvDbSc114
     C                   endsl
      *
     C                   endsr
      *
      *====================================================================
      * Move data base fields to screen fields - extra for CTL114
      *====================================================================
     C     MvDbSc114     begsr
      *
     C                   eval      scNote = *blank
     C                   eval      scmedi = *blank
     C     keyIMDNL3     setll     pdn03
     C                   dou       %eof(IMDNL3)
     C     keyIMDNL3     reade     pdn03
     C                   if        not %eof
     C                              and DNIUSR <> 'PDSPGMR'
     C                              and DNPRFL = *blanks
     C                   eval      scNote = '*'
     C                   leave
     C                   endif
     C                   enddo
     c                   eval      xxindk = *blanks
     c                   eval      scmedi = *blanks
     c                   movel     iedkey        xxindk
      *
     c                   if        (IEPNAM = 'W0070R')
      *
     c                   call      'FA700R'
     c                   parm      *blanks       #rtcd             1
     c                   parm      xxco#         #co#              3
     c                   parm      xxfnd         #wcf#             3
     c                   parm      *blanks       #sub#             5
     c                   parm      *blanks       #mcf#             3
      *
     c                   endif                                                  IEPNAM = 'W0070R'
      *
     c                   movel     xxco#         xxco#1            3 0
     c                   movel     #sub#         #sub#1            5 0
     c                   movel     #mcf#         #mcf#1            3 0
     c                   movel     xxfyr         xxfyr1            3 0
     c                   movel     xxcase        xxcase1           7 0
     c                   movel     iedcid        xxdcid           12
      *
     c     kyau1         chain     pau01                              60
     c                   if        *in60 = *off
     c                   if        aureas <> *blanks
     c                   movel     '+'           scmedi
     c                   endif                                                  aureas <> *blanks
      *
     c                   endif                                                  *in60 = *off
      /free

       if view# = 5 or view# = 6;
       for i1 = 1 to #ofHd;
         %subst(hd(i1):%len(scHdL1)-4) = ' ' + 'N' + '  ' + 'M' ;
       endfor;
       %subst(scData2:%len(scData2)-4) = ' ' + scNote + '  ' + scmedi;
       else;
       for i1 = 1 to #ofHd;
         %subst(hd(i1):%len(scHdL1)-4) = ' ' + ' ' + '  ' + 'N' ;
       endfor;
       %subst(scData2:%len(scData2)-1) = ' ' + scNote;
       endif;
      /end-free
     C                   endsr
      *
      *====================================================================
      * Opt6 - Process option 6
      *====================================================================
     C     Opt6          begsr
      *
     C                   eval      #6RtCd = '      '
      *
     C                   select
     C                   when      scFl01 = 'F'
     C                   eval      #6RtCd = 'F     '
     C                   callp(e)  PrintFax(scDcId
     C                                     :scPth
     C                                     :fill40
     C                                     :'00000'
     C                                     :' '
     C                                     :#6RtCd
     C                                     :##QID
     C                                     :scUncN
     C                                     :scIfsP
     C                                     )
     C                   if        %error
     C                   callp(e)  PrintFax(scDcId
     C                                     :scPth
     C                                     :fill40
     C                                     :'00000'
     C                                     :' '
     C                                     :#6RtCd
     C                                     :##QID
     C                                     )
     C                   endif
     C                   other
     C                   callp     PrintImg(scDcId
     C                                     :scPth
     C                                     :fill40
     C                                     :'00000'
     C                                     :#6RtCd
     C                                     :' '
     C                                     :##QID
     C                                     )
     C                   endsl
      *
     C                   select
     C                   when      #6RtCd = '1     '
     C                   eval      error = *on
     C                   when      #6RtCd = '2     '
     C                   eval      error = *on
     C                   when      #6RtCd = '3     '
     C                   eval      error = *on
     C                   when      #6RtCd = '4     '
     C                   eval      error = *on
     C                   when      #6RtCd <> '     0' and
     C                             #6RtCd <> '000000' and
     C                             #6RtCd <> '      '
     C                   eval      error = *on
     C                   other
     C                   eval      error = *off
     C                   endsl
      *
     C                   if        not error
     C                   clear                   scOpt
     C                   endif
      *
     C                   endsr
      *
      *====================================================================
      * Opt7 - Process option 7
      *====================================================================
     C     Opt7          begsr
      *
     C                   callp     Notes(scDcId
     C                                  :q1Pgm
     C                                  )
      *
     C                   clear                   scOpt
      *
     C                   endsr
      *
      *====================================================================
      * Opt8 - Process option 8
      *====================================================================
     C     Opt8          begsr
      *
      * Read the file IMIELF based on scqid/scdcid/scsdt/scstm
     c     kyie1         chain     pie01f
     C                   if        %found
     C                   select
     C                   when      IEFL02 = *blank
     C                   eval      IEFL02 = 'Y'
     C                   eval      *in16  = *on
     C                   other
     C                   eval      IEFL02 = *blank
     C                   eval      *in16  = *off
     C                   endsl
     C                   update    pie01f
     C                   endif
     C*
     C                   clear                   scOpt
      *
     c                   call      'IS215R4'
     c                   parm      kkqid         xxqid            10
     C                   endsr
      *
      *====================================================================
      * Opt9 - Process option 9
      *====================================================================
     C     Opt9          begsr
      *
     C                   callp     Split(scQID
     C                                  :scDcId
     C                                  :%editc(CvtSDtMDYY(scSDt):'Z')
     C                                  :%editc(scSTm:'X')
     C                                  :scDOCT
     C                                  :##RtCd
     C                                     )
      *
      /Free

       Select;
       when ##RtCd = 'A'; // F3;
         Exsr PgmEnd;
         Return;
       when ##RtCd = 'C'; // F12;
         *inkl = *on;
       when ##RtCd = '*'; // Submitted;
         xxData2 = '**Split Submitted**';
         clear scOpt;
       Endsl;

      /End-free
     C                   Endsr
      *
      *====================================================================
      * Opt#2 - Process option 2 (Alternate)
      *====================================================================
     C     Opt#2         begsr
      *
      * Clear Reassign subfile
     C                   sub       sfrrn4        sfrrn4               14
     C                   eval      *in13 = *on
     C                   write     scr04c
     C                   eval      *in13 = *off
      *
     C                   dou       %eof
     C                   select
     C                   when      scSel = '#2'
     C                   eval      scsf1r = sfrrn1
     C                   add       1             sfrrn4               14
     C                   clear                   scSel
     C                   write     scr04s
     C                   when      scSel <> '#2' and scSel <> '2'
     C                   eval      *in08 = *on
     C                   endsl
     C                   update    scr01s
     C                   eval      *in08 = *off
     C                   readc     scr01s
     C                   enddo
      *
      * Process reassignment requests
     C                   eval      scrrn4 = 1
     C                   select
     C                   when      not opt2Single
     C                   write     scr04
     C                   exfmt     scr04c
     C                   other
     C                   eval      *inkl = *off
     C                   endsl
     C                   eval      firstReassign = *on
      *
     C                   if        *inkl = *off
      *
     C                   dou       %eof
     C                   readc     scr04s
     C                   if        not %eof
     C                   select
     C                   when      opt2Single
     C                   eval      ##RtCd = ' '
     C                   when      firstReassign
     C                   eval      ##RtCd = 'P'
     C                   eval      firstReassign = *off
     C                   other
     C                   eval      ##RtCd = '*'
     C                   endsl
     C                   callp     Reassign(%editc(CvtSDtMDYY(scSDt):'Z')
     C                                     :%editc(scSTm:'X')
     C                                     :scDocT
     C                                     :scDcId
     C                                     :scFl01
     C                                     :scFlr
     C                                     :scSFlr
     C                                     :scPth
     C                                     :scQID
     C                                     :scDesc
     C                                     :##RtCd
     C                                     )
     C                   select
     C                   when      ##RtCd = 'A'
     C                   exsr      PgmEnd
     C                   return
     C                   when      ##RtCd = 'B'
     C                   eval      *inkl = *on
     C                   leave
     C                   when      ##RtCd = '#'
     C     scsf1r        chain     scr01s
     C                   eval      *in33 = *on
     C                   clear                   scOpt
     C                   eval      scData2 = '**Not Reassigned**'
     C                   eval      xxData2 = '**Not Reassigned**'
     C                   update    scr01s
     C                   other
     C     scsf1r        chain     scr01s
     C                   eval      *in33 = *on
     C                   clear                   scOpt
     C                   eval      scData2 = '**Reassigned**'
     C                   eval      xxData2 = '**Reassigned**'
     C                   update    scr01s
     C                   endsl
     C                   endif
     C                   enddo
      *
      * Close reassign program
     C                   eval      ##RtCd = 'L'
     C                   callp     Reassign('00000000'
     C                                     :'000000'
     C                                     :scDocT
     C                                     :scDcId
     C                                     :scFl01
     C                                     :scFlr
     C                                     :scSFlr
     C                                     :scPth
     C                                     :scQID
     C                                     :scDesc
     C                                     :##RtCd
     C                                     )
     C                   endif
      *
     C                   write     scr01
     C                   eval      count2 = 0
     C                   eval      *in14 = *off
     C     scsf1r        chain     scr01s
      *
     C                   endsr

      *====================================================================
      * Opt#4 - Process option 4 (Alternate)
      *====================================================================
      /Free

       Begsr Opt#4;

          //  Call the API to remove document and write notes.
          Remove(%editc(CvtSDtMDYY(scSDt):'X'):%editc(scSTm:'X'):scDcId
             :##QID:q1Pgm:'N':##RtCd);

          Select;
          when ##RtCd = 'L'; // Locked;
            *in07 = *on;
            *in05 = *on;
            *in72 = *on;
            *in90 = *on;
          When ##RtCd = 'I' or ##RtCd = 'D'; // Removed;
            Clear scOpt;
            svRtCd = ##RtCd;
            xxData2 = '**Remove**';
          Endsl;

        EndSr;

      /End-free

      *====================================================================
      * Opt#5 - Process option 5 (Alternate)
      *====================================================================
     C     Opt#5         begsr
      *
     C                   select
     C                   when      scFl01 = 'F'
     C                   callp     DspFax(scDcId
     C                                   :scPth
     C                                   :##RtCd
     C                                   )
     C                   other
     C                   callp     DspImg(scDcId
     C                                   :scFlr
     C                                   :scSFlr
     C                                   :scDocT
     C                                   :scDesc
     C                                   :##RtCd
     C                                   )
     C                   endsl
      *
     C                   call      sc10A0        a0pl                   99
      *
     C                   clear                   scOpt
      *
     C                   endsr
      *
      *====================================================================
      * Opt#10 - Process option 10 Preview at the image queue
      *====================================================================
     C     Opt#10        begsr
      *
     C                   select
     C                   when      scFl01 = 'F'
     C                   callp     DspFax(scDcId
     C                                   :scPth
     C                                   :##RtCd
     C                                   )
     C                   other
     C                   callp     DspImg(scDcId
     C                                   :scFlr
     C                                   :scSFlr
     C                                   :scDocT
     C                                   :scDesc
     C                                   :##RtCd
     C                                   )
     C                   endsl
      *
     C                   clear                   scOpt
      *
     C                   endsr
      *
      *====================================================================
      * OthFks - Process other function keys
      *====================================================================
     C     OthFks        begsr
      *
     C     *inkg         caseq     *on           KgDo
     C     *in10         caseq     *on           HelpDo
     C                   endcs
      *
     C                   endsr
      *
      *====================================================================
      * OthOpt - Process other options
      *====================================================================
     C     OthOpt        begsr
     C     scSel         caseq     '7'           Opt7                     90
     C     scSel         caseq     '8'           Opt8                     90
     C     scSel         caseq     '9'           Opt9                     90
     C     scSel         caseq     '#2'          Opt#2                    90
     C     scSel         caseq     '#4'          Opt#4                    90
     C     scSel         caseq     '#5'          Opt#5                    90
     C     scSel         caseq     '10'          Opt#10                   90
     C                   endcs
      *
     C                   clear                   ##RtCd
      *
     C                   endsr
      *
      *====================================================================
      * PgmEnd - End of program processing
      *====================================================================
     C     PgmEnd        Begsr
      *
     C                   Select
     C                   When      ##RtCd <> *blank
     C                   Eval      @@RtCd = ##RtCd
     C                   When      *inkc
     C                   Eval      @@RtCd = 'A'
     C                   Eval      *inlr = *on
     C                   When      *inkl
     C                   Eval      outflg = *blanks
     C                   Eval      rstpgm = *on
     C                   Exsr      PgmStr
     C                   Endsl
      *
      /free
       //*** Begin Add *** 10/05/17 ******************
       //*** Begin Chg *** 04/10/19 ************************************
       //*** If cltID = '114';
       //*** Begin Chg *** 07/10/19 ************************************
       //*** If cltID = '114' Or cltID = '152';
       If (cltID = '114' Or cltID = '152') And *inLR = *On;
       //*** End   Chg *** 07/10/19 ************************************
       //*** End   Chg *** 04/10/19 ************************************
         If %Open(faaul1);
            Close faaul1;
         EndIf;
       EndIf;
       //*** End   Add *** 10/05/17 ******************
      /end-free
      *
     C                   Endsr
      *
      *====================================================================
      * PgmStr - Start of program processing
      *====================================================================
     C     PgmStr        Begsr
      /Free

       Callp RtvCltID(cltID);

       scPrd4 = CenterFld('Confirm Reassignment of ' + optNm:%size(scPrd4));

       Select;
       When %parms >= 3;
         @@Call = @#Call;
       Other;
         @@Call = *blank;
       Endsl;

       ##RtCd = *blank;
       ##QID = *Blanks;
       Select;
       When @@RtCd = 'S'; // Work with Shared Queues
         Callp WWSharedQs(##QID:fill40:##RtCd);
       When @@RtCd = 'L'; // Work with All Queues
         Callp WWAllQs(##QID:fill40:##RtCd);
       When @@QID <> *blank;
         ##QID = @@QID;
       Endsl;

       Select;
       When ##RtCd = 'A';
         *Inlr = *on;
         Exsr PgmEnd;
         Return;
       When ##QID = *blanks;
         ##QID = q1User;
       Endsl;

       kkQID = ##QID;
       scUsrQ = ##QID;

       viewMx = 6;
       Clear DRFL15;
       Chain 'SC' sdr01;
       Select;
       When DRFL15 = 'Y';
         viewMx = 4;
         view# = 4;
       Endsl;

       Select;
       //*** Begin Chg *** 04/10/19 ************************************
       //*** When cltID = '114';
       When cltID = '114' Or cltID = '152';
       //*** End   Chg *** 04/10/19 ************************************
         //*** Begin Add *** 10/05/17 ******************
         If Not %Open(faaul1);
            Open faaul1;
         EndIf;
         //*** End   Add *** 10/05/17 ******************
         Exsr Update114;
       Endsl;

      /End-free
     c                   call      'IS215R4'
     c                   parm      kkqid         xxqid            10
     c                   eval      *in12 = *on
     c                   exsr      clrget
     C                   endsr
      *
      *====================================================================
      * Read   - Read next record from file
      *====================================================================
     C     Read          begsr
    C*
     C                   select
     C                   when      view# = 1
     C     key           reade     dbrcdf                                 95
     C                   when      view# = 2
     C     key           reade     dbrcdf2                                95
     C                   when      view# = 3
     C     key           reade     dbrcdf3                                95
     C                   when      view# = 4
     C     key           reade     dbrcdf4                                95
     C                   when      view# = 5
     C     key           reade     dbrcdf5                                95
     C                   when      view# = 6
     C     key           reade     dbrcdf6                                95
     C                   endsl
      *
     C                   endsr
      *
      *====================================================================
      * ReadP  - Read previous record from file
      *====================================================================
     C     ReadP         begsr
      *
     C                   select
     C                   when      view# = 1
     C     key           readpe    dbrcdf                                 95
     C                   when      view# = 2
     C     key           readpe    dbrcdf2                                95
     C                   when      view# = 3
     C     key           readpe    dbrcdf3                                95
     C                   when      view# = 4
     C     key           readpe    dbrcdf4                                95
     C                   when      view# = 5
     C     key           readpe    dbrcdf5                                95
     C                   when      view# = 6
     C     key           readpe    dbrcdf6                                95
     C                   endsl
      *
     C                   endsr
      *
      *====================================================================
      * ResetSel - Reset scSel from scOpt when an error is displayed
      *====================================================================
     C     ResetSel      begsr
      *
     C                   eval      svInd = *in
     C                   movea     '0000000000'  *in(71)
     C                   movea     '0000000000'  *in(81)
      *
     C                   eval      svRRN1 = sfRRN1
     C                   eval      *in08 = *on
     C                   update    scr01s
     C                   eval      *in08 = *off
      *
     C                   write     scr01c
      *
     C                   dou       %eof
     C                   readc     scr01s
     C                   if        not %eof
     C                   if        svRRN1 > sfRRN1
     C                   eval      scSel = scOpt
     C                   endif
     C                   eval      *in08 = *on
     C                   update    scr01s
     C                   eval      *in08 = *off
     C                   endif
     C                   enddo
      *
     C                   write     scr01c
      *
     C     svRRN1        chain     scr01s
      *
     C                   eval      *in = svInd
      *
     C                   endsr
      *
      *====================================================================
      * SetGT  - Set greater than on file
      *====================================================================
     C     SetGT         begsr
      *
     C                   select
     C                   when      view# = 1
     C     key1          setgt     dbrcdf                             96
     C                   when      view# = 2
     C     key2          setgt     dbrcdf2                            96
     C                   when      view# = 3
     C     key3          setgt     dbrcdf3                            96
     C                   when      view# = 4
     C     key4          setgt     dbrcdf4                            96
     C                   when      view# = 5
     C     key5          setgt     dbrcdf5                            96
     C                   when      view# = 6
     C     key6          setgt     dbrcdf6                            96
     C                   endsl
      *
     C                   endsr
      *
      *====================================================================
      * SetLL  - Set lower limits on file
      *====================================================================
     C     SetLL         begsr
      *
     C                   select
     C                   when      view# = 1
     C     key1          setll     dbrcdf                             96
     C                   when      view# = 2
     C     key2          setll     dbrcdf2                            96
     C                   when      view# = 3
     C     key3          setll     dbrcdf3                            96
     C                   when      view# = 4
     C     key4          setll     dbrcdf4                            96
     C                   when      view# = 5
     C     key5          setll     dbrcdf5                            96
     C                   when      view# = 6
     C     key6          setll     dbrcdf6                            96
     C                   endsl
      *
     C                   endsr
      *
      *====================================================================
      * Sf1Upd - Update subfile record when item is deleted
      *====================================================================
     C     Sf1Upd        begsr
     C                   endsr
      *
      *====================================================================
      * SlctRd - Move fields for selected record to parameters
      *====================================================================
      /Free

       Begsr SlctRd;

       Callp     IndexRcd(scQID
                         :scDcId
                         :%editc(CvtSDtMDYY(scSDt):'Z')
                         :%editc(scSTm:'X')
                         :scDocT
                         :##RtCd
                         );

       sl1Flg = *off;
       Clear xxData2;
       Select;
       When ##RtCd = 'L'; // Locked;
         Eval *in07 = *on;
         Eval *in05 = *on;
         Eval *in72 = *on;
         Eval *in90 = *on;
       When ##RtCd = 'C'; // F12;
         Eval *in90 = *on;
       When ##RtCd = '3'; // F3;
         sl1Flg = *on;
       When ##RtCd = '8'; // Rejected;
         xxData2 = '**Rejected**';
       When ##RTCD = '9'; // Reassigned;
         xxData2 = '**Reassigned**';
       When ##RTCD = 'P'; // Bypassed;
       Other;             // Indexed;
         xxData2 = '**Indexed**';
       Endsl;

       If *in90;
         Exsr ResetSel;
       Endif;

       Endsr;

      /End-free
      *
      *====================================================================
      * Subset - Determine if record should be put in subfile
      *====================================================================
     C     Subset        begsr
     C                   endsr
      *
      *
      *====================================================================
      * Updates needed for CLT114
      *====================================================================
     C     Update114     Begsr
      /Free

       xxOpFn = '1';
       xxNmbr = '08';
       xxMIS  = 'YNN';
       Exsr UpdOFA;

       For i1 = 1 to #ofHd;
         %subst(hd(i1):%len(scHdL1)) = 'N';
       Endfor;

      /End-free
     C                   Endsr

      *====================================================================
      * ENKEDO - Exit point for before KEDO processing
      *====================================================================
     C     ENKEDO        begsr
     *
      * Call program IS215R4 to re-load the workfile IXIEP
     c                   call      'IS215R4'
     c                   parm      kkqid         xxqid            10
      *
     C                   endsr
      *====================================================================
      * EXKEDO - Exit point for after KEDO processing
      *====================================================================
     C     EXKEDO        begsr
     *
     *
     C                   endsr
      ***** END ADD ******* 01/25/12 **********************************
      *====================================================================
      * AlphaTime - Convert numeric time to alpha time
      *====================================================================
     PAlphaTime        b
     DAlphaTime        pi             8
     D inTime                         6  0 const
     D*
      /Free

       Test(te) *hms inTime;
       Select;
       When %error;
         Return %char(inTime);
       Other;
         Return %char(%time(inTime:*hms):*hms);
       Endsl;

      /End-free
     PAlphaTime        e

      *====================================================================
      * CvtSDtMDYY - Convert from YYMD to MDYY
      *====================================================================
     PCvtSDtMDYY       b
     DCvtSDtMDYY       pi             8  0
     D inDate                         8  0 const
     D*
     D outDate         s              8  0
      *
      /Free

       Test(de) *iso inDate;
       Select;
       When %error;
         Return inDate;
       Other;
         outDate = %dec(%char(%date(inDate:*iso):*usa0):8:0);
         Return outDate;
       Endsl;

      /End-free
     PCvtSDtMDYY       e

      *====================================================================
      * CvtSDtYYMD - Convert from MDYY to YYMD
      *====================================================================
     PCvtSDtYYMD       b
     DCvtSDtYYMD       pi             8  0
     D inDate                         8  0 const
      *
     D date8           s              8  0
      *
      /Free

       date8 = inDate;
       date8 = DtCheck(date8);
       Test(de) *usa date8;
       Select;
       When %error;
         Return inDate;
       Other;
         Return %dec(%char(%date(date8:*usa):*iso0):8:0);
       Endsl;

      /End-free
     PCvtSDtYYMD       e

      *====================================================================
      * IndexCd - Return code needed for indexing
      *====================================================================
     PIndexCd          b
     DIndexCd          pi             1
     D inFlag                         1
      *
      /Free

       Select;
       When inFlag = 'F';
         Return 'F';
       Other;
         Return 'I';
       Endsl;

      /End-free
     PIndexCd          e
** Column headings
Opt     Date      Time   Document ID  Doc Type  Description
Opt  Document ID    Date       Time   Doc Type  Description
Opt  Doc Type  Document ID    Date       Time   Description
Opt   Rcv Date   Document ID   Doc Type         Description
Opt  Entity Name          DOI/EFD    DOC ID     Description                           ** 01/25/12
Opt  Index Key            DOI/EFD    DOC ID     Description                           ** 01/25/12
Opt     Date      Time   Document ID  Doc Type  Comment
Opt  Document ID    Date       Time   Doc Type  Comment
Opt  Doc Type  Document ID    Date       Time   Comment
Opt   Rcv Date   Document ID   Doc Type         Comment
Opt  Entity Name          DOI/EFD    DOC ID     Comment                               ** 01/25/12
Opt  Index Key            DOI/EFD    DOC ID     Comment                               ** 01/25/12
** Description--------------MISN
1011=Index entry            YNN
1022=Reassign               YNN
1044=Remove                 YNN
1055=View Document          YYY
1066=Print                  NNN
1077=Notes                  YYY
1088=In Progress?           NNN
1099=Split                  YYY
11010=Preview               YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
207F7=Auto process          YNN
209F9=Change info           YYY
211F11=Change view          YYY
212F12=Cancel               YYY
215F15=Fold/Truncate        YNN                                                      ** 01/25/12
3
