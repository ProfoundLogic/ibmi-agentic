
     H DFTACTGRP(*NO) ACTGRP(*NEW)
     fmmnvl     if   e           k disk    rename(pnv01:pnv00)
     fsmupl     if   e           k disk    rename(pup01:pup00)

     D PUIATEXCFG      PR                  ExtPgm('PUIATEXCFG')
     D  iUser                       128A   Const
     D  oSettings                          LikeDS(Settings)
     D  oError                         N
     D  oErrorMsg                   256A

     D PUIATEXCFG      PI
     D  iUser                       128A   Const
     D  oSettings                          LikeDS(Settings)
     D  oError                         N
     D  oErrorMsg                   256A

     D PUIINITLIB      PR                  ExtPgm('PUIINITLIB')

     D GetHomePage     PR                  ExtPgm('PUIATEXHOM')
     D  userId                       10
     D  homePageCode                  9  0

     D UpdateUserParm  PR                  ExtPgm('W0007R')

     D Settings      E DS                  ExtName(ATCONFIGP)

     D item_DS         DS                  qualified
     D   MNAM                  1     10
     D   OPTN                 11     14
     D   item                  1     14

     d userid          s                   like(upusr)
     D homePageCode    s                   like(NVitem)

     D/copy sPrPrprty

      /FREE

        PUIINITLIB();

        oError = *Off;
        oErrorMsg = '';

        Monitor;
          UpdateUserParm();
        On-Error;
        EndMon;

        oSettings.ACID = 0;                        // Ignored.

        // Browser document title.
        oSettings.ACBTITLE = %Trim(SysProperty('AtBrowserTitle'));
        If oSettings.ACBTITLE = *Blanks;
          oSettings.ACBTITLE = 'StoneRiver';
        Endif;

        // 1/0 show banner.
        oSettings.ACBAN = %Trim(SysProperty('AtShowBanner'));
        If oSettings.ACBAN = *Blanks;
          oSettings.ACBAN = '1';                     // 1/0 show banner.
        Endif;

        // Banner height in pixels.
        Monitor;
          oSettings.ACBANHGHT = %Int(%Trim(SysProperty('AtBannerHeight')));
        On-Error;
          oSettings.ACBANHGHT = 50;
        EndMon;
        If oSettings.ACBANHGHT = 0;
          oSettings.ACBANHGHT = 50;
        Endif;

        // Banner HTML file URL.
        oSettings.ACBANURL = %Trim(SysProperty('AtBannerURL'));
        If oSettings.ACBANURL = *Blanks;
          oSettings.ACBANURL = '/profoundui/userdata/html/atrium_banner.html';
        Endif;

        // Theme name. Blank for default.
        oSettings.ACTHEME = %Trim(SysProperty('AtTheme'));

        // 1/0 allow user to change theme.
        oSettings.ACUTHEME = %Trim(SysProperty('AtAllowThemeChange'));
        If oSettings.ACUTHEME = *Blanks;
          oSettings.ACUTHEME = '1';
        Endif;

        // 1/0 show home page.
        oSettings.ACSHOWHOME = %Trim(SysProperty('AtShowHomePage'));
        If  oSettings.ACSHOWHOME = *Blanks;
          oSettings.ACSHOWHOME = '1';
        Endif;

        // Home page menu item code/URL
        oSettings.ACHOMEURL = '';
        userid = iUser;
        GetHomePage(userId:homePageCode);
        If homePageCode > 0;
          Chain(n) ('H':homePageCode) pnv00;
          If %Found(mmnvl);
            Select;
            When NVactntype = '0';
              oSettings.ACHOMEURL = NVactn;                 // Home page menu item code.
            When NVactntype = '2';
              item_DS.mnam = NVmnam;
              item_DS.optn = NVoptn;
              oSettings.ACHOMEURL = item_DS.item;           // Home page menu item code.
            Endsl;
          Endif;
        Endif;
        //If oSettings.ACHOMEURL = *Blanks;
        //oSettings.ACHOMEURL = %Trim(SysProperty('AtDefaultHomePage'));
        //Endif;

        // Navigation panel title.
        oSettings.ACNTITLE = %Trim(SysProperty('AtPanelTitle'));
        If oSettings.ACNTITLE = *Blanks;
          oSettings.ACNTITLE = 'Menu';
        EndIf;

        // Navigation panel width in pixels.
        Monitor;
          oSettings.ACNWIDTH = %Int(%Trim(SysProperty('AtPanelWidth')));
        On-error;
          oSettings.ACNWIDTH = 250;
        EndMon;
        If oSettings.ACNWIDTH = 0;
          oSettings.ACNWIDTH = 250;
        Endif;

        // T/A tree or accordion menu style.
        oSettings.ACNTYPE = %Trim(SysProperty('AtMenuStyle'));
        If oSettings.ACNTYPE = *Blanks;
          oSettings.ACNTYPE = 'A';
        Endif;

        // 1/0 single click to launch item.
        oSettings.ACSCLICK = %Trim(SysProperty('AtSingleClick'));
        If oSettings.ACSCLICK = *Blanks;
          oSettings.ACSCLICK = '1';
        Endif;

        *InLr = *On;
        Return;

      /END-FREE

