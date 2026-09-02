# J.H. Fletcher — Demo Script

For the screens now live in `FLTDEMO`: **Parts Finder**, **Fleet 360**, **Duplicate part review**
and the **Production Schedule Board**. Roughly 12 minutes for the first two, another 8 for the
schedule board. Read §1 before you drive it.

---

## 1. Before anyone is watching

**Add `FLTDEMO` to the demo profile's initial library list.** Otherwise you have to type
`ADDLIBLE FLTDEMO` on the menu command line in front of the customer, which looks like the demo isn't
finished. This is the single most likely thing to embarrass you.

**Hard-refresh the browser once.** The Genie skin's `start.html` carries the EJS shim and is ~3 MB —
browsers cache it aggressively.

**Check your browser window height.** Grids show up to 15 rows and scroll inside themselves, sized so
the footer buttons always stay reachable. On a short window you get fewer rows — around 8 at 640px
against 15 at 1000px. If you're presenting on a projector at a low resolution, maximise the window and
hide browser toolbars before you start, or you'll be scrolling more than you'd like.

**Then walk it once, end to end, in the room you'll present from.** Not the day before — the same
morning, on the same network.

**Any column heading sorts the grid.** Click to sort, click again to reverse. Dates sort
chronologically, money and percentages numerically. Useful mid-demo: on Fleet 360, sort by
**Modernization** to bring the FULL DUE machines to the top, or on the customer lookup sort by
**Trend** to put the worst-declining accounts first. It's a good way to answer "which is the worst
one?" without leaving the screen.

**Double-click is the standard selection everywhere.** Any row with a pointer cursor can be
double-clicked to take its default action — show the duplicate cluster, pick the machine, open machine
detail. Typing the option number still works if you prefer, and it's what you'll fall back to for the
non-default options like `7`.

**Know the two numbers you're going to say out loud:**
- Parts Finder opens on machine **FL004236** with **21 of 36 parts flagged** as having near-duplicates
- Fleet 360 opens on **Cumberland Ridge Mining LLC** at **-72.6%** parts spend with **6** machines due

If those numbers have changed because the data was reseeded, read the new ones off the screen before
you start. Never quote a number you haven't just looked at.

---

## 2. The one thing to say first

> "Two things before I start. This is **mocked data** — the customer names are invented, the serial
> numbers are invented. And these are screens we built this week to show you a direction, not a
> finished product. What's real is that they're running on an IBM i, in RPG, against DDS files, with
> no change to how your system works."

Say this unprompted. Ray spotted invented fleet data in the last demo and asked where it came from.
Volunteering it is what buys you credit for everything you *don't* caveat — and both screens carry a
`DEMO DATA — FICTIONAL RECORDS` badge so nobody has to take your word for it.

---

## 3. Parts Finder — the Julia demo

**Navigate:** main menu → `4` → `1`

The screen opens **already populated** on an HDDR Twin Boom Roof Drill. Don't type anything yet.

### Beat 0 — "how would I know the serial?"

Somebody will ask this, and it's the right question. Answer it before they do:

> "You wouldn't. A parts call starts with a company name, not a serial number."

Click **Look up machine…**. It opens on the **whole installed base** — 43 machines across every
customer and site. Let that land for a second; it's the first time in the demo they see the scale of
what's out there.

Then type `Cumberland` and Enter. Six machines, with model, customer, site, build date and status.
**Double-click one.**

> "Start with everything, narrow to the customer. Company name, mine site, model, or a partial serial
> — any of them finds it. That's how the call actually arrives."

Worth knowing: if someone types a company name straight into the **Machine serial** box, that isn't a
dead end either — it hands the text to the lookup and searches it. Nothing you type is wasted.

Then press on. The rest of the demo runs from whichever machine you picked — the numbers below are for
the default machine, so **read yours off the screen.**

There's a second route worth showing later, in §4: from Fleet 360, option `7` against any machine jumps
straight here with that serial loaded. That's the real workflow — *find the customer, pick their
machine, see its parts* — and it's worth doing in that order if you have time for only one pass.

### Beat 1 — this machine, not this model

> "This is one machine. Serial FL004236, an HDDR Twin Boom Roof Drill, built 2019, at Redstone
> Minerals' Gilsonite Works. And look at this line — **options fitted**: automated temp roof support,
> extended operator canopy, remote control, wet drilling."

Point at the parts table.

> "That parts list isn't the model's parts list. It's *this* serial number's parts list. A part that
> needs the wet drilling package only appears because this machine has it. You build every machine to
> order — so the only parts list that's any use is the one for the machine as it was actually built."

**Why this beat matters:** it establishes you understand engineer-to-order before you make any claim
about their data. If you skip it, everything after sounds generic.

### Beat 2 — the duplicate problem, visible before you click anything

The parts list is grouped by assembly, and the duplicates now land as **consecutive rows inside the
same assembly**. You don't need the cluster panel to make the point — just read the screen:

> "Drill head. Three rows: DRILL CHUCK 200 SERIES, DRILL CHUCK 200, CHUCK DRILL 200 SER. Same chuck.
> Three part numbers. And look at the prices — eighteen hundred, thirty-two hundred, thirty-eight
> hundred dollars. For the same part. Because they've been priced independently for years."

Then the boom assembly, three rows down:

> "Same thing again. Boom cylinder 88, three ways, three prices."

**This is stronger than the cluster panel** because nobody had to go looking for it. It's just sitting
on the screen the moment the machine loads.

Point at the red bar.

> "Twenty-one of these thirty-six parts have near-duplicate part numbers somewhere else in the item
> master."

Then let it sit for a second, and hand it to Julia:

> "Julia, you told us engineers create new part numbers because the same thing has been named a dozen
> different ways over the years and looking it up is too onerous. This is that, counted."

### Beat 3 — the money shot

**Double-click** the **`HP-A1-STD` — HYDRAULIC PUMP (STANDARD)** row.

Read the panel out loud, slowly, oldest first:

| Part number | Description | Created | Status |
|---|---|---|---|
| `HYD-PUMP-A1` | HYDRAULIC PUMP ASSY | 1998 | Superseded |
| `HYDPUMPA1` | HYD PUMP ASSEMBLY | 2007 | Active |
| `PUMP-HYD-A1` | PUMP, HYDRAULIC, ASSY | 2014 | Active |
| `HP-A1-STD` | HYDRAULIC PUMP (STANDARD) | 2021 | Active |

> "Four part numbers. One physical pump. Four different naming conventions, adopted across twenty-three
> years. Three of them are still active — so three of them can still be ordered, stocked and quoted.
> And the prices don't agree with each other, because they've been maintained independently."

The cluster panel adds what the grid can't show: the **creation dates**, so you can see the naming
conventions arriving one era at a time, and which number superseded which.

Then the point:

> "Nothing here is blocked and nothing is merged. It tells the person doing the work, at the moment
> they're doing it. That's deliberate — a hard stop is what people route around, and routing around it
> is how you got four part numbers in the first place."

**That last sentence is the whole argument.** It answers Julia's *"people circumvent the system because
they can"* without contradicting her.

### Beat 4 — make it theirs

F3 back. Type a search term into **Search within this machine** — `PUMP`, `FILTER`, or `VALVE` — Enter.

> "Filtered inside this machine's fitment. Not the whole catalogue."

Then hand over the keyboard:

> "Give me a word and I'll search it."

If Matthew or Nick is in the room, this is the moment to let one of them drive. A junior person
succeeding unaided in front of the CEO is worth more than anything you can say.

---

## 4. Fleet 360 — the Chuck and Rod demo

**Navigate:** F3 → `2`

Opens on Cumberland Ridge Mining LLC.

### Beat 0 — the whole installed base, by account

Click **Look up customer…** before you do anything else. Eight accounts, each with machine count,
sites, parts spend and year-over-year trend. Three of the eight are down more than 20%, shown in red.

> "That's your installed base by account, with parts spend and the trend. Three of these eight accounts
> are buying materially less from you than they were last year. Nobody has to drill in to see it."

**This is the strongest single screen for Chuck and Rod**, and it takes ten seconds. Then double-click
Cumberland Ridge and carry on with the beats below.

### Beat 1 — the KPI strip

> "Six machines, three sites. Parts spend last full year $72,542 against $265,584 the year before.
> That's **down 72%**. And six of the six are past their modernization interval."

### Beat 2 — the sentence that matters

Point at the account summary.

> "Six machines across three sites. Six due for modernization review. Parts spend down 72% year over
> year — possible service gap."

Then, deliberately:

> "That line is generated from the machine table, the service table and the parts-order table. Every
> number in it comes from a query. If you ask me where the 72% came from, I can show you the SQL."

**Do not skip this.** In the last demo the equivalent panel was model-written prose over invented data,
and *"that was a great demo, but what's actually there?"* is the exact objection you're now working
against. The screen even says so in a footnote under the summary — point at it.

### Beat 3 — the CFO number

> "This is one account. You have hundreds. If nobody is running this query, then modernization revenue
> is being identified by the customer phoning you — which means you find out about a machine when
> they've already decided to do something about it."

That is the sentence for Chuck and Rod. It reframes the demo from *IT tidiness* to *revenue you're
not capturing.*

### Beat 4 — drill down, and close the loop

**Double-click** the first machine.

> "Ownership, build and ship dates, hours, last service, last modernization and type — and the full
> service history with technician, downtime hours and cost."

> "That's the machine record a modernization conversation needs. On the phone. Right now."

F3 back, then type **`7`** against the same machine — options you type still work; double-click is
just the shortcut for the common one.

> "And that's the parts that fit it. Same machine, no retyping. Customer calls, you find them, you pick
> their machine, you're looking at its parts list — that's one path through two screens."

This is the beat that makes it feel like an application rather than two demos.

F3 back out.

---

## 4b. Production Schedule Board — the Mike demo

`FLTMENU` option 4. This is the one that replaces something they can point at: the physical
sticky-note schedule. Budget 8 minutes.

### Beat 0 — say what it is before they ask

> *"This is your build schedule. Every machine on the floor, where it is, and when we think it
> actually ships. Invented data — but the shape is your shape: work centres, routings, standard
> hours, material shortages."*

### Beat 1 — the lane strip is the bottleneck, in one glance

The row of work-centre cards is load in **working weeks of queued work**, coloured green through red.
Point at the spread: the front of the shop (Teardown, Fabrication) is green at 3–4 weeks; the back
half (Blast, Hydraulic, Electrical, Assembly, Test) is red at 10–16 weeks.

> *"The shop average is 8.5 weeks of routed work. But Electrical is sixteen. That's not a capacity
> problem, it's a **sequencing** problem, and nobody can see it on a corkboard."*

**Click a lane** to filter the grid to the orders with work left at that centre. Click the chip next
to the dropdown to clear it.

**Point at WELD2.** It says *no capacity* in red, with an **Inactive** chip — and it still has two
orders and 600-odd hours queued on it. That's a real finding, and it leads straight into Beat 3.

### Beat 2 — promised against projected

The **Ship window** column is a 300-day track: black tick is the promised date, orange is where the
schedule actually lands, the pink bar between them is the slip, and the thin grey line is today.

Sort by it, or just read the top of the grid — it opens with the trouble first: past-promised, then
projected-to-miss, then the ones you cannot say anything about at all.

**Double-click a row.** Work order detail: the routing operation by operation, standard against
actual hours with the variance, and any operation waiting on material shaded red with the part, the
quantity short, the vendor and the PO.

**The yellow sticky note in the corner is the point.** That is `FLWNOTE`, free text, straight off
the board — *"Second shift approved on FAB through month end"*, *"Trade-in core arrived short"*.

> *"We didn't structure that field. That's the note as it exists today. The system doesn't need to
> understand it to stop it being the only place the information lives."*

### Beat 3 — the honest panel ⭐ do not skip this

The orange panel at the top says **schedule confidence** and a percentage. Click **Review data
confidence**.

Fourteen checks, run against the data every time the screen opens. On the seeded data: **40 findings
across 23 of 44 work orders, 16 of which invalidate a projected date outright.** Impossible promised
dates. Orders in process with no start date. Operations reported complete with no hours booked.
Material overdue with no arrival date. Work queued at a work centre that has no crew.

> *"Mike told us you're plugging this data into scheduling applications and finding a lot of database
> issues. He's right, and this is what that looks like. We are not going to show you a board that
> pretends otherwise — a schedule that quietly invents its inputs is worse than no schedule."*

**Double-click a finding** to land on the work order it is on. The detail screen repeats it in plain
language next to its own projected date.

Then show one that refuses to answer: a work order with no routing shows a blank projected date, a
`NO DATA` flag, and *"This work order has no routing operations at all, so it carries no hours and
no capacity plan can see it. That is the finding, not a display problem."*

> *"That order is consuming your shop and no plan can see it. We could have put a plausible date
> there. We'd rather tell you."*

### What this beat buys you

It is the answer to *"how do we know it's right?"* asked before they ask it, and it turns the data
quality problem from something IT is embarrassed about into something with a number, an owner and a
work queue. It also sets up the live B4 database health scan on their real data: same idea, their
system.

---

## 5. What to say when they push

**"How long did this take?"**
> "Days, not weeks — and the point isn't the screens, it's that the RPG underneath didn't change. Same
> files, same programs, same platform."

**"Is this real or is it a mock-up?"** *(Mike will ask this)*
> "The screens are real and running on your platform. The *data* is invented — I said so up front.
> Point us at a copy of your item master and we'll run the same duplicate analysis against your real
> part numbers. That's a read-only query. You'd know by the end of the week how big the problem is."

**That is the close.** It converts the demo into a concrete, low-risk, read-only next step — and the
answer is a number they've never had.

**"Could Infor do this?"**
> "Infor will give you a clean item master on day one and a process to keep it clean. What it won't do
> is tell you which of your existing four part numbers is the right one — that's your product knowledge,
> eighty-eight years of it, and it has to be untangled either way. Doing that before a migration makes
> the migration cheaper. Doing it instead of a migration is a different conversation. Either way this
> work has value."

Don't fight the ERP. Rod needs a path where saying yes here isn't reversing himself in public.

**"What happens when it breaks?"**
Don't argue — show them. This is where the controlled-failure demo belongs, not here.

---

## 6. Be straight about what isn't built

If asked, say so plainly. Getting caught overstating costs more than the gap does.

- **No quote basket.** You can see parts; you can't add them to a quote and send it. This is the most
  obvious next thing to build and worth saying so — it invites them to ask for it.
- **Duplicate detection is description-based** — abbreviation expansion, number words, noise words,
  then word and size signatures. It's tunable and it will produce some false positives on real data.
  That's the correct behaviour for a review tool, but say it rather than let them find it.
- **Fitment is model plus option code**, not true serial-level BOM.
- **Read-only.** Nothing writes back.
- **Modernization thresholds are assumptions** (full at 15 years, partial at 7). Ask them what the real
  intervals are — it's a good question to leave the room with.
- **The schedule board is read-only and does not reschedule.** It projects; it will not let you drag
  an order or reorder the queue. Say so before someone tries.
- **The capacity model is deliberately simple**: one clock per work centre, one order at a time at
  that centre's daily hours, loaded in promised-date order. Real finite-capacity scheduling has
  alternate routings, overlapping operations and setup times. This is honest about what it is, and
  the assumption is one sentence — which is the point.
- **The confidence scan's fourteen checks are ours, not theirs.** Ask which ones they'd add. That is
  the best question in the whole demo, because the answer is a specification.

---

## 7. Run of show

| | Screen | Beat | For |
|---|---|---|---|
| 0:00 | — | "This is mocked data" | Everyone |
| 0:30 | Parts Finder | **Look up machine** — search "Cumberland", pick one | Everyone |
| 1:15 | | Machine identity; options fitted | Mike, Julia |
| 2:15 | | N of M parts flagged | Julia |
| 3:00 | | Option 5 — the four-pump cluster | Julia, Mike |
| 5:00 | | Search; hand over the keyboard | Matthew / Nick |
| 6:30 | Fleet 360 | **Look up customer** — 8 accounts, 3 declining | Chuck, Rod |
| 7:00 | | KPI strip for the chosen account | Chuck, Rod |
| 8:00 | | "Every number comes from a query" | Mike, Ray |
| 9:00 | | Revenue-not-captured framing | Chuck, Rod |
| 10:00 | | Option 5 — machine + service history | Ray, Mike |
| 10:30 | | Option 7 — jump to parts for that machine | Julia, Mike |
| 11:00 | — | The close: run it on your real item master | Julia, Mike |

Add the schedule board when you have the time for it — it plays best straight after Fleet 360,
because both are "your business, computed" and it hands off naturally into the live B4 database
health scan:

| | Screen | Beat | For |
|---|---|---|---|
| 11:30 | Schedule Board | Lane strip — 8.5 weeks average, Electrical at 16 | Mike, Julia |
| 12:30 | | WELD2: inactive, and still has work queued on it | Mike |
| 13:00 | | Ship window — promised against projected | Chuck, Rod |
| 14:00 | | Double-click: routing, variances, material block | Mike, Ray |
| 15:00 | | The sticky note, carried through as free text | Everyone |
| 16:00 | | **Review data confidence** — 40 findings, 16 blocking | Mike, Ray |
| 17:30 | | An order it refuses to give a date for | Mike |
| 18:00 | — | Hand off to the live B4 scan on their real data | Mike, Ray |

---

## 8. If you remember one thing

The screens are not the demo. **The four-member pump cluster is the demo** — it's their own problem,
counted, on screen, in under three minutes. Everything else is context for that moment.

And the close is not "buy the platform." It's *"let us run this read-only query against your real item
master and tell you how big it actually is."* That's a yes they can give in the room.

If you show the schedule board, it has a second one of these: **the confidence panel is the demo**,
not the board. Anyone can draw a schedule. Saying out loud which of your own dates you don't believe,
and why, with a number, is the thing no ERP demo will do for them.
