Name: Tucker Brock

Description:
------------
This is my implementation of the DateTime language
(a.k.a. "The TimeZone Machine"). DateTime is a small
domain-specific language (DSL) for working with time
zone conversions and time calculations.
The interpreter parses and evaluates expressions like:
- `convert "2025-04-22 15:30" from "US/Central" to "Asia/Tokyo"`
- `now in "UTC"`
- `now in "US/Eastern" + 3 hours + 15 minutes`
- `"Europe/London"`
- `difference "GMT" "Zulu"`

My implementation includes:
- Parsing and evaluation of datetime expressions
- Time zone conversions using the tz database
- Time arithmetic (add/subtract hours, minutes, seconds)
- Dynamic evaluation using system time for real-time queries
- Helpful error handling for bad dates or bad time zones
- Ability to compute the UTC offset for any time zone
- Ability to compute the time difference between two zones

The code is thoroughly commented and follows clear, 
consistent Haskell style.

Files:
------------
DateTime.hs
- Houses parser, evaluator, and core logic of the DSL.
DateTimeREPL.hs
- Allows for interactive use of the DSL using similar
  logic to previous class projects.
Parsing2.hs
- Similar to Quilt and Calc projects; home of parsing.
datetime.cabal
- Location of executable `datetime`.
stack.yaml
LICENSE

Effort:
------------
To reach Level 2, I added user-friendly error handling
throughout the evaluator, as previously stated. I also 
safely and correctly handled nested addition and subtraction
expressions.

Notably challenging features/moments included:
- Safely validating time zones...
  Haskell's tz package does not support access to a list of
  all time zones for reference or to use in error handling.
- Spending significant effort using `catch` and `IOException` 
  to safely load time zones in a manner that kept the rest of
  the code functional and clean.
- Researching `time` and `tz` packages...
  Although they expedited the process in many ways, the time
  spent researching and encountering its limitations required
  many hours of time.
- Designing and implementing `ShowOffset` and `Difference`...
  Despite library constraints, requesting the offset or 
  finding the difference between two zones became possible. 

Extensions:
------------
The following extensions went beyond the original project plan:
- Safe error handling of bad time zone input
- Flexible nested expression handling
- Support for determining UTC offsets
- Ability to compute time differences between zones

Examples:
------------
- `convert "2025-04-22 15:30" from "US/Central" to "Asia/Tokyo"`
- `now in "UTC"`
- `convert "2025-05-07 07:10" from "Europe/Paris" to "Africa/Addis_Ababa" - 3 hours`
- `now in "asia/urumqi" + 8 hours + 20 minutes`
- `"Asia/Kolkata"`
- `difference "US/Pacific" "us/eastern"`
