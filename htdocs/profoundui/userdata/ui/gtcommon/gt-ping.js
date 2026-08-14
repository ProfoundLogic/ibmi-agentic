/* ==================================================================
 * gt-ping -- deliberately empty. The URL is the message.
 *
 * gt-scan.js fetches this file with everything it knows in the QUERY STRING, and
 * the HTTP server writes that query string into its access log:
 *
 *     ssh dev "grep gt-ping /www/drpuidev/logs/access_log.Q1YYMMDD00 | tail"
 *
 * So a device reports its version, user agent, decoder path, frame count and last
 * error without anybody screenshotting anything -- and without a CGI program, a
 * database table, an RPG change or any authority I do not have.
 *
 * It exists because six rounds of one camera bug were argued from descriptions
 * while the answer -- which build is actually on the device -- was unknowable from
 * here. The access log already recorded every request; it just had nothing useful
 * to record. Now it does.
 *
 * The file must exist and must be served (200), or the browser logs a 404 and the
 * request still reaches the log but reads as an error. Nothing here executes.
 * ================================================================== */
