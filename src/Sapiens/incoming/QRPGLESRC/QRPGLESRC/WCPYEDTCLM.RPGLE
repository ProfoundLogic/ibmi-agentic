      *=============================================================================================
      *  Convert to PDF
      *=============================================================================================
     c     CvtToPDF      begsr

     C     *dtaara       Define    *lda          dirPDF
     C                   in        *dtaara

     C                   if        dirPDF <> *blanks
     C* QSYSPRT
     C                   callp(e)  runCmd('SPLAMATIC/SPLCONVERT '
     C                                  + 'FILE(QSYSPRT) '
     C                                  + 'TOOBJ(''/' + %trim(dirPDF)
     C                                  + '/' + %trim(q1PGM) + '.pdf'') '
     C                                  + 'TRANSFORM(*PDF811)'
     C                                  :255)
     C* EDTCLMS
     C                   callp(e)  runCmd('SPLAMATIC/SPLCONVERT '
     C                                  + 'FILE(EDTCLMS) '
     C                                  + 'TOOBJ(''/' + %trim(dirPDF)
     C                                  + '/' + %trim(q1PGM) + '.pdf'') '
     C                                  + 'TRANSFORM(*PDF811)'
     C                                  :255)
     C* EDTPROV
     C                   callp(e)  runCmd('SPLAMATIC/SPLCONVERT '
     C                                  + 'FILE(EDTPROV) '
     C                                  + 'TOOBJ(''/' + %trim(dirPDF)
     C                                  + '/' + %trim(q1PGM) + '.pdf'') '
     C                                  + 'TRANSFORM(*PDF811)'
     C                                  :255)
     C* EDTWMCDPO
     C                   callp(e)  runCmd('SPLAMATIC/SPLCONVERT '
     C                                  + 'FILE(EDTWMCDPO) '
     C                                  + 'TOOBJ(''/' + %trim(dirPDF)
     C                                  + '/' + %trim(q1PGM) + '.pdf'') '
     C                                  + 'TRANSFORM(*PDF811)'
     C                                  :255)
     C* EDTWMCMPO
     C                   callp(e)  runCmd('SPLAMATIC/SPLCONVERT '
     C                                  + 'FILE(EDTWMCMPO) '
     C                                  + 'TOOBJ(''/' + %trim(dirPDF)
     C                                  + '/' + %trim(q1PGM) + '.pdf'') '
     C                                  + 'TRANSFORM(*PDF811)'
     C                                  :255)
     C* EDTPMTS
     C                   callp(e)  runCmd('SPLAMATIC/SPLCONVERT '
     C                                  + 'FILE(EDTPMTS) '
     C                                  + 'TOOBJ(''/' + %trim(dirPDF)
     C                                  + '/' + %trim(q1PGM) + '.pdf'') '
     C                                  + 'TRANSFORM(*PDF811)'
     C                                  :255)
     C                   endif

     c                   endsr
