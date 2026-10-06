-- WIPJOBS : Work-in-process jobs (WIPJOBH is the history access path over the same layout)
CREATE OR REPLACE TABLE WIPJOBS (
  WPNN     CHAR(2)        NOT NULL DEFAULT ''        , -- Plant (key 1)
  WPJOB    DECIMAL(8,0)   NOT NULL DEFAULT 0         , -- Job number (key 2)
  WPID     DECIMAL(5,0)   NOT NULL DEFAULT 0         , -- Customer id
  WPIT     CHAR(15)       NOT NULL DEFAULT ''        , -- Item
  WPNNOV   CHAR(2)        NOT NULL DEFAULT ''        , -- Override plant
  WPIDOV   DECIMAL(5,0)   NOT NULL DEFAULT 0         , -- Override customer id
  WPITOV   CHAR(15)       NOT NULL DEFAULT ''        , -- Override item
  WPJOBP   CHAR(1)        NOT NULL DEFAULT ''        , -- 'P' = parent job
  WPORDQ   DECIMAL(9,3)   NOT NULL DEFAULT 0         , -- Ordered quantity
  WPSHIQ   DECIMAL(9,3)   NOT NULL DEFAULT 0         , -- Shipped quantity
  WPADD    CHAR(1)        NOT NULL DEFAULT ''        , -- 'A' = added
  WPRORN   CHAR(1)        NOT NULL DEFAULT ''        , -- 'R' = reorder
  WPENTD   DECIMAL(7,0)   NOT NULL DEFAULT 0         , -- Entered CYMD
  WPOLDD   DECIMAL(7,0)   NOT NULL DEFAULT 0         , -- Previous due CYMD
  WPNEWD   DECIMAL(7,0)   NOT NULL DEFAULT 0         , -- Current due CYMD
  WPCYMD   DECIMAL(7,0)   NOT NULL DEFAULT 0         , -- Last change CYMD
  WPCHMS   DECIMAL(6,0)   NOT NULL DEFAULT 0         , -- Last change HHMMSS
  WPUSER   CHAR(10)       NOT NULL DEFAULT ''        , -- Last changed by
  WPUSEX   DECIMAL(9,3)   NOT NULL DEFAULT 0         , -- Cost multiplier (from MASTSPEP.MQUSEX)
  WPDEL    CHAR(1)        NOT NULL DEFAULT ''        , -- 'D' = deleted
  WPX      CHAR(1)        NOT NULL DEFAULT ''        , -- 'X' = cancelled
  PRIMARY KEY (WPNN, WPJOB)
)
RCDFMT WPFMT;

LABEL ON TABLE WIPJOBS IS 'Work In Process Jobs';
