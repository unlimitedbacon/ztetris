# ZTetris

The best TI-83+ Tetris ported to KnightOS.

[Original](http://www.ticalc.org/archives/files/fileinfo/94/9489.html) by Jimmy Mårdell  <yarin@acc.umu.se>
Sam Heald <evil_sam@hotmail.com>
Patrick Davidson <eeulplek@hotmail.com>

ION and Ti-83/+ Version by Ahmed El-Helw <ahmed.ticalc.org>

## Compiling

First, install the [KnightOS SDK](http://www.knightos.org/sdk).

    $ knightos init
    $ make
    $ make run # to test
    $ make package # to produce an installable package

## Installing

Use `make package` to get a package that you can install.

## Help, Bugs, Feedback

If you need help with KnightOS, want to keep up with progress, chat with
developers, or ask any other questions about KnightOS, you can hang out in the
IRC channel: [#knightos on irc.freenode.net](http://webchat.freenode.net/?channels=knightos).
 
To report bugs, please create [a GitHub issue](https://github.com/KnightOS/KnightOS/issues/new) or contact us on IRC.
 
If you'd like to contribute to the project, please see the [contribution guidelines](http://www.knightos.org/contributing).


FEATURES
--------
 * 21 levels (0-20). You can choose which level to start between
   (only start on level 0-9 though).

 * 16 different level patterns!

 * Hiscore table with names! The three best results are saved.

 * Teacher key! If you see your techer heading your way, quickly
   press the teacher key to return to TI-OS, and when you start
   the game the next time, you'll automatically resume where you
   was. If you run it from a shell however, it returns to the
   shell instead. But that's your problem, not mine :)

 * Linkplay! Play against each other and kick some butt! The link
   protocol is the same as in ZTetris for the 86 and Tetris for Fargo,
   so you could link an 82 to an 85, 86, or a 92 and play against each other!

 * The scoring is almost the same from the Game Boy version. The last digit
   in the score isn't shown (you could think of it that way anyway) because
   of several reasons. Unlike the ZShell version, you also score for
   dropping or moving down a piece.

 * You can start with trashlines. It's different from the gameboy version
   though, because there is no win-when-cleared-25-lines.

 * In linkplay, you can choose if you want to get scrambled (as in Tetris
   for Windows) or unscrambled (Tetris for Gameboy) lines.

 * Rotation in both direction. You can pause the game (1 player)

CONTROL
-------
```
Choosing players:    Up/Down      - Change number of players
                     1/2          - Choose that number of players
                     Clear        - Quit

At the option menu:  Arrowkeys    - Choose level
                     Enter        - Start level
                     S            - Toggle "2-4 lines", "1-3 lines" (LP only)
                     L            - Toggle scrambled/unscrambled lines (LP)
                     Trace        - Decrease start high
                     Graph        - Increase start high
                     Clear        - Quit game

When playing:        Left & Right - Move the piece to the left and right
                     Up & 2nd     - Rotate clockwise
                     Down         - Move the piece faster down
                     xto          - Drop key
                     Alpha        - Rotate anti-clockwise
                     Clear        - Quit game
                     Mode         - Pause
                     Del          - Teacher key. Quickly jumps out to
                                    TI-OS. The game will resume when you
                                    start it the next time.

When typing name:    A-Z, Space   - Enter letter (max 10 chars)
                     Enter        - Continue
                     Left arrow   - Backspace
```

SCORING
-------
|          | L 0  | L 1 | L 2 | L 3 | L 4 | L 5 | L 6 | L 7 | L 8  | L 9  |
| -------- | ---- | --- | --- | --- | --- | --- | --- | --- | ---- | ---- |
| 1 line   |    4 |   8 |  12 |  16 |  20 |  24 |  28 |  32 |   36 |   40 |
| 2 lines  |   10 |  20 |  30 |  40 |  50 |  60 |  70 |  80 |   90 |  100 |
| 3 lines  |   30 |  60 |  90 | 120 | 150 | 180 | 210 | 240 |  270 |  300 |
| 4 lines  |  120 | 240 | 360 | 480 | 600 | 720 | 840 | 960 | 1080 | 1200 |

When dropping a piece, your score will increase with dropsteps/10 (the
last digit will remain in memory, although not shown). Every time you
move the piece down (with the down key) you will increase the score with
0.1.

LINKPLAY
--------
To play linkplay, first connect two TI-83's (or connect the TI-83 to a
TI-82, TI-85, TI-86, or a TI-92) and choose two player mode. The level selection
menu looks slightly different. Besides from choosing which level to start at
(the two players can start at different levels), you can choose how many
penalty lines should be sent to the other calc when clearing lines. Either
you send 2-4 lines (if you get 2-4 lines) or 1-3, ie one line less than you
cleared. This option can also be different on both calcs, thus allowing
handicap. You can also toggle if you want to get unscrambled penalty lines
or scrambled penalty lines. If choosing unscramble (defualt), the lines sent
to you will only have one gap, making it much easier than getting totally
scrambled lines.

Press Enter to start the game. Now the calc will go into waiting mode and
wait until the other player has reached this mode. When one player enters
the waiting mode, the other player MUST have started ZTetris (or be in an
assembly shell) because else the game would start immediately for the first
player. If you link with a TI-82 or a TI-85 or a TI-86 or a TI-92, the other calc must enter Waiting mode first.

Now the fun begins!

You play as you usually do, but when you clear 2 or more lines, the other
opponent will get penalty lines depening on how many lines you cleared and
the option choosed when selecting startlevel.

There is also a bar telling how high up in the playing field your opponent
is (it's not visible in the beginning since the field is empty). This is
good when choosing strategy.

When a player loses, or aborts the game, a "You won" message will appear
on the other calc.

You can't pause or use the teacher key in two player mode, and you can
neither enter the hiscore table (even though score is counted)

When you play against an 85/86 know that the 82 version runs slightly
faster than those versions. Set the 85/86, one level higher than the 82
to even things out.

KNOWN BUGS
----------
 * Don't pull out the linkcable while playing twoplayer! The calcs
   may freeze a few seconds (not crash).
 * The linkplay probably isn't that stable, and the calc may freeze
   sometimes for a short time. It is also possible that one player
   may "miss" some penalty lines :) See this as a feature instead hehe :)

VERSION HISTORY
---------------
ION Version 1.1 R3
-Fixed random bugs

ION Version 1.1 R2  * Released by Ahmed El-Helw again :)
- Fixed the link routines, thanks Dan! [dan@calc.org, http://tcpa.calc.org]

ION Version 1.1 *Released by Sam Heald
- Fixed the speed (exactly like 82,86 versions)
- Fixed Blinking text bug (no more black line)
- Changed pause screen to shut down after ~1 minute (battery saver)
- Fixed a few crashing bugs (not Ahmed's fault, TI changed the
    inputs and outputs of a few ROM calls)
- Size: 3341 bytes


83+ Version 1.0 R4
- Fixed the Mode bug
- ION 1.1 is out, should fix screen problems
- 2 player.. not working yet... if you can fix it, go for it but don't release it
  without talking to me first.

83+ Version 1.0 R3
- Fixed a bug in the pause screen
- If the screen resizes, it's a problem with Joe's copy routine.. wait for next ION version.

83+ Version 1.0 R2
- Fixed 1 player mode
- Screen may resize in midgame, this is because of Joe's copy routine [next verion oF ION]
- Slowed the game down a bit
- Let me know if there are any more bugs with this...

83+ Version 1.0
- Only 1 Player Works [2 Player Mode Displays Waiting...]
- May crash due to the copy routine [wait till next version of ION]




THANKS
------
Special thanks to...
 Pascal Bouron   - great link routines!
 Dan E - Fixing the 83+ ilnk rotuines
 Martin Hock     - example source on how to use those link routines
 CalcEm Author   - His emulator was a huge help!
 Ahmed El-Helw   - beta testing and helped with backgrounds
 Joe Wingbermule - helped with backgrounds
 Mark May (OBD)  - beta testing and trying to help
 Dines Christy Justesen - Trying to help
 Derrick Ward    - beta testing, trying to help when possible, and maintaining our page.
 Bryan Rabeler &
  Kouri Rosenberg - For repeatedly crashing their calcs for good of Ztetris.
   And the rest of the beta testers...
....and most of all
 Patrick Davidson - For teaching me everything I know about asm, and
                    solving the major bugs that I(Sam) could not.

Ti-83 Thanks...
 Florent Dhordain - the link routines!
 Joe Wingerbermuhle - the faster graph buffer copy [and fixing it!] 
 Djpupon, Dustin Graham, and BlackBelt - for reporting the bugs
 My Beta Testers - for suggestions.

------------------------------------------------------------------------------
 
HOW TO CONTACT (SAM) ABOUT THE 82 VERSION
-----------------
The best way is to send an email to evil_sam@hotmail.com.

  Visit my webpage at www.cswnet.com/~ddward/index.html for the latest info.


HOW TO CONTACT (PATRICK) ABOUT THE 82 VERSION
----------------
      WWW - http://www.calweb.com/~kwdavids/patrick/
            http://www.toptown.com/hp/ariwsi/
    EMail - ariwsi@juno.com
            ariwsi@rocketmail.com
            eeulplek@hotmail.com

HOW TO CONTACT (Ahmed) about 83 Version
----------------
http://ahmed.ticalc.org
ahmed@ticalc.org

HOW TO CONTACT (JIMMY) ABOUT ANY OTHER VERSION OF ZTETRIS
-----------------
The easiest way is to send an email to mja@algonet.se, but you can also
find me on IRC, #calc-ti at EFnet with the nick Yarin. I'm usually there
about 11:00 pm - 02:00 am CET (5:00-8:00 EST).
IRC:         Yarin
Email:       mailto:yarin@acc.umu.se
Homepage:    http://www.acc.umu.se/~yarin/

------------------------------------------------------------------------------

//Ahmed El-Helw <ahmed@ticalc.org / http://ahmed.ticalc.org>
//Sam Heald <evil_sam@hotmail.com / http://void.calc.org>
//Jimmy M†rdell <yarin@acc.umu.se / http://www.acc.umu.se/~yarin>
//Patrick Davidson <eeulplek@hotmail.com>
