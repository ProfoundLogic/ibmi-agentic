      *
      *  Customer Maintenance
      *
      *=======================================================================
     H DFTACTGRP(*NO)

     FCUSTMNTD  cf   e             workstn
     F                                     sfile(sfl1:rrn1)
     F                                     sfile(sfl2:rrn2)
     F                                     infds(info)
     F/IF DEFINED(WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F/ENDIF


     FCUSTLF    if   e           k disk    rename(pfr:lfr)
     FCUSTPF    uf a e           k disk
     FSTATEL2   uf a e           k disk

     D QCMDEXC         PR                  ExtPgm('QCMDEXC')
     D  command                   32000A   Const Options(*VarSize)
     D  commandlen                   15P 5 Const

     D command         S             50A

     Dinfo             ds
     D cfkey                 369    369

     Dexit             C                   const(X'33')
     Dcancel           C                   const(X'3C')
     Dadd              C                   const(X'36')
     Denter            C                   const(X'F1')
     Drollup           C                   const(X'F5')
     Dsflpag           C                   const(12)
     Ddisplay          C                   const('5')
     Dchange           C                   const('2')
     Ddelete           C                   const('4')
     Dreports          C                   const(x'B4')
     Dmessages         C                   const(x'B5')

     Dlstrrn           S              4  0 inz(0)
     Dlstrrn2          S              4  0 inz(0)
     Dcount            S              4  0 inz(0)
     Dnew_id           S                   like(dbidnm)
      *
      *****************************************************************
      *  Main Routine
      *****************************************************************
      *
      * Clear then build the initial subfile
      *
     C                   exsr      clrsf1
     C                   exsr      sflbld
     C
      *
      * Do loop to process the subfile until F3 is pressed.  If F12
      * is pressed from other screens, I still want to stay in this loop.
      *
     C                   dou       cfkey = exit
      *
     C                   write     fkey1
     C                   exfmt     sf1ctl
      *
      * Process position to information entered by the user, then clear
      * and rebuild the subfile. Lastly, clear the position to field
      *
     C                   select
      *
      * Add more records to subfile if user pages from bottom of subfile
      *
     C                   when      (*in60) and (not *in32)
     C                   exsr      sflbld
     C
     C                   when      (cfkey = enter) and (ptname <> *blanks)
     C     ptname        setll     custlf
     C                   exsr      clrsf1
     C                   exsr      sflbld
     C                   clear                   ptname
      *
      * Process screen to interrogate options selected by user
      *
     C                   when      (cfkey = enter) and (ptname = *blanks)
     C                   exsr      prcsfl
      *
      * User presses F6, throw the add screen, clear, and rebuild subfile
      *
     C                   when      cfkey = add
     C                   movel(p)  'Add   '      mode
     C                   exsr      addrcd
     C     *loval        setll     custlf
     C                   exsr      clrsf1
     C                   exsr      sflbld

     C                   when      cfkey = reports
     C
     C                   Eval      command = 'WRKSPLF ASTLVL(*BASIC)'
     C                   CallP     QCMDEXC(command : %Len(%Trim(command)))
     C
     C                   when      cfkey = messages
     C
     C                   Eval      command = 'WRKMSG ASTLVL(*BASIC)'
     C                   CallP     QCMDEXC(command : %Len(%Trim(command)))

     C                   when      cfkey = cancel
     C                   leave

     C                   endsl
     C                   enddo

     C                   eval      *inlr = *on
      *
      *****************************************************************
      *  CLRSF1 - Clear the subfile
      *****************************************************************
      *
     C     clrsf1        begsr
      *
      * Clear relative record numbers and subfile
      *
     C                   eval      rrn1 = *zero
     C                   eval      lstrrn = *zero
     C                   eval      *in31 = *on
     C                   write     sf1ctl
     C                   eval      *in31 = *off
     C                   eval      *in32 = *off

     C                   endsr
      *
      *****************************************************************
      *  CLRSF2 - Clear the subfile
      *****************************************************************
      *
     C     clrsf2        begsr
      *
      * Clear relative record numbers and subfile for confirmation screen
      *
     C                   eval      rrn2 = *zero
     C                   eval      lstrrn2 = *zero
     C                   eval      *in41 = *on
     C                   write     sf2ctl
     C                   eval      *in41 = *off

     C                   endsr
      *
      *****************************************************************
      *  SFLBLD - Build the List
      *****************************************************************
      *
     C     sflbld        begsr
      *
      * Make RRN1 = to the last relative record number of the subfile
      * so that the load process will correctly add records to the bottom
      *
     C                   eval      rrn1 = lstrrn
      *
      * Load the subfile with one page of data or until end-of-file
      *
     C                   do        sflpag
     C                   read      custlf                                 90
     C                   if        *in90
     C                   leave
     C                   endif
     C                   eval      rrn1 = rrn1 + 1
     C                   eval      option = ' '
     C                   eval      *in74 = *off
     C                   write     sfl1
     C                   enddo
      *
      * If no records are loaded to subfile, don't display it
      *
     C                   if        rrn1 = *zero
     C                   eval      *in32 = *on
     C                   endif
      *
     C                   eval      lstrrn = rrn1
      *
     C                   endsr
      *
      *****************************************************************
      *  PRCSFL - Process the options selected
      *****************************************************************
      *
     C     prcsfl        begsr
      *
      * Clear the confirmation subfile before starting
      *
     C                   exsr      clrsf2
      *
      * Read all the subfile records that were changed by the user
      *
     C                   readc     sfl1
      *
      * Do loop to process until all changed records are read
      *
     C                   dow       not %eof

     C                   select
      *
      * when a 5 is entered throw the DISPLAY screen
      * until F3 or F12 is pressed on that screen
      *
     C                   when      option = display
     C                   movel(p)  'Display'     mode
     C     dbidnm        chain(n)  custpf
     C                   if        %found
     C                   exfmt     panel2
     C                   endif
     C                   eval      option = *blank
     C                   update    sfl1
     C                   if        (cfkey = exit) or (cfkey = cancel)
     C                   leave
     C                   endif
      *
      * when a 2 is entered execute the update subroutine,
      * blank out the option field, and update the subfile record
      *
     C                   when      option = change
     C                   movel(p)  'Update'      mode
     C                   exsr      chgdtl
     C                   eval      option = *blank
     C                   update    sfl1
     C                   if        (cfkey = exit) or (cfkey = cancel)
     C                   leave
     C                   endif
      *
      * when a 4 is entered write the record the the confirmation screen,
      * set on the SFLNXTCHG indicator to mark this record as changed,
      * and update the subfile.  I mark this record incase F12 is pressed
      * from the confirmation screen and the user wants to keep his
      * originally selected records
      *
     C                   when      option = delete
     C                   eval      rrn2 = rrn2 +1
     C                   write     sfl2
     C                   eval      *in74 = *on
     C                   update    sfl1
     C                   eval      *in74 = *off

     C                   endsl

     C                   readc     sfl1
     C                   enddo
      *
      * If records were selected for delete (4), throw the subfile to
      * screen.  If enter is pressed execute the DLTRCD subroutine to
      * physically delete the records, clear, and rebuild the subfile
      * from the last deleted record (you can certainly position the
      * database file where ever you want)
      *
     C                   if        rrn2 > 0
     C                   eval      lstrrn2 = rrn2
     C                   eval      rrn2 = 1
     C                   write     fkey2
     C                   exfmt     sf2ctl
     C                   if        (cfkey <> exit) and (cfkey <> cancel)
     C                   exsr      dltrcd
     C     dblnam        setll     custlf
     C                   exsr      clrsf1
     C                   exsr      sflbld
     C                   endif
     C                   endif

     C                   endsr
      *
      *****************************************************************
      *  CHGDTL - allow user to change data
      *****************************************************************
      *
     C     chgdtl        begsr
      *
      * chain to data file using selected subfile record
      *
     C     dbidnm        chain     custpf
      *
      * If the record is found (it better be), throw the change screen.
      * If F3 or F12 is pressed, do not update the data file
      *
     C                   if        %found
     C                   Dou       *in30 = *Off
     C                   exfmt     panel1
     C                   Eval      *In30 = *Off
     C                   if        (cfkey <> exit) and (cfkey <> cancel)
     C                   if        DBSTATE <> ''
     C     DBSTATE       Chain     STATER
     C                   If        Not %Found()
     C                   Eval      *In30 = *On
     C                   EndIf
     C                   EndIf
     C                   EndIf
     C                   EndDo

     C                   if        (cfkey <> exit) and (cfkey <> cancel)
     C                   update    pfr
     C                   endif

     C                   endif

     C                   endsr
      *
      *****************************************************************
      *  ADDRCD - allow user to add data
      *****************************************************************
      *
     C     addrcd        begsr
      *
      * set to last record in the the file to get the last ID number
      *
     C     *hival        setgt     custpf
     C                   readp     custpf
      *
      * set a new unique ID and throw the screen
      *
     C                   if        not %eof
     C                   eval      new_id = dbidnm + 1
     C                   else
     C                   Eval      new_id = 1
     C                   endif
     C                   clear                   pfr
     C                   eval      dbidnm = new_id
     C
     C                   Dou       *in30 = *Off
     C                   exfmt     panel1
     C                   Eval      *In30 = *Off
     C                   if        (cfkey <> exit) and (cfkey <> cancel)
     C                   if        DBSTATE <> ''
     C     DBSTATE       Chain     STATER
     C                   If        Not %Found()
     C                   Eval      *In30 = *On
     C                   EndIf
     C                   EndIf
     C                   EndIf
     C                   EndDo
      *
      * add a new record if the pressed key was not F3 or F12
      *
     C                   if        (cfkey <> exit) and (cfkey <> cancel)
     C                   write     pfr
     C                   endif

     C                   endsr
      *
      *****************************************************************
      *  DLTRCD - delete records
      *****************************************************************
      *
     C     dltrcd        begsr
      *
      * read all the records in the confirmation subfile
      * and delete them from the data base file
      *
     C                   do        lstrrn2       count
     C     count         chain     sfl2
     C                   if        %found
     C     dbidnm        delete    pfr                                99
     C                   endif
     C                   enddo

     C                   endsr
