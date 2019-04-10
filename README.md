# The First Terrorists Virus (TTV1)

## Facts
- TTV1 was the first file-based Amiga virus
- Its creation was a fun experiment. It was inspired by the boot block based [SCA virus](https://www.sca.ch/amiga/virus/sca-virus.html) which had reached dubious fame due to its very successful spreading and the flaw of overwriting the boot block. Would it be possible to create a self spreading file, thus breaking out of the boot block dread?
- Development took place between spring and fall 1988.
- The early code name was *HOUZ* virus.
- Thinking up the name we tried to find something that would sound mean and at the same time on the verge to ridiculous. The final name was inspired by a rap band called The Terrorists.
- The virus does not harm the system. In fact, care was taken to avoid any even unintentional damage.
- Identity of the authors was kept secret for 30 years. It was officially revealed on March 9, 2019 at the [Demonights 008 event in Bern Switzerland](https://slack-files.com/T0K11HLNQ-FFFCSC7N3-6f346eb498
)
- "The names have been changed to protect the innocent" shown in the message was inspired by the song [Beat Dis by Bomb the Bass](https://en.wikipedia.org/wiki/Beat_Dis). They have sampled it from the [Dragnet radio series](https://en.wikipedia.org/wiki/Dragnet_(radio_series)).
- The ["BGS9"](http://agn-www.informatik.uni-hamburg.de/catalog/amiga/html/bgs91.htm) was in fact a clone. Its code is almost identical even including the resident name "TTV1". It got more reach and thus was regarded as the origin.

## About the life of the beast
1. TTV1 installs itself as a reset proof resident module (KickTag/ROMTag) named "TTV1"
1. On reset the resident module (virus) is called
1. Execution gets delayed in order to gain disk write access. Therefore Intuition's OpenWindow function is redirected.
1. As soon as the AmigaDOS tries to open the CLI window the OpenWindow gets called the virus looks for the startup-sequence on the booted disk
1. The virus is looking for the first command (*A*) in the startup-sequence
1. File request windows (e.g. "Disk is write protected") are being temporarily disabled
1. Virus renames *A* to $a0202020a02020a020a0a0. This is a combination of none breaking spaces and spaces. The idea is that the user will oversee this "invisible" file.
1. Virus writes itself as an executable command with the name of *A* to the disk
1. Eventually *A* gets loaded and executed with all its parameters by the virus

The next time the user will boot the disk the virus gets loaded into RAM and step 1 takes place. On reset the horizonal beam position gets evaluated. If the beam position is smaller than a certain number the screen turns black and a [message](#message-screen) in white letters appears.

## Running
Use [Masterseka](http://www.pouet.net/search.php?what=masterseka&type=prod) (e.g. V1.71) to assemble/compile the source code (ttv1.s) and run the virus.

```
SEKA>r ttv1.s
File length=xxxxx  (=$0000xxxx)
SEKA>a
OPTIONS>
No Errors
SEKA>j
```
Then (soft) reset your Amiga to activate the virus.

## Authors
* [Amicom](https://twitter.com/friedepeace)
* [Depeche](https://twitter.com/roger_wetzel)

## Acknowledgments
- TTV1 was inspired by [SCA (Mega-Mighty Swiss Cracking Association) virus](https://www.sca.ch/amiga/virus/sca-virus.html)
- Code snippet of how to disable file request windows was provided by [Zodiac](http://janeway.exotica.org.uk/author.php?id=5548)

## Message Screen
The evolution of the message screen while development.

![Early TTV1 message 1](https://roger-wetzel.github.io/images/ttv1_pre1.png)

_Static text using the default system font._


![Early TTV1 message 2](https://roger-wetzel.github.io/images/ttv1_pre2.gif)

_Faded text lines using the default system font._


![Final TTV1 message](https://roger-wetzel.github.io/images/ttv1.gif)

_Final version: Faded text lines using a custom designed 8-bit font._

## References on TTV1
* http://agn-www.informatik.uni-hamburg.de/catalog/amiga/html/bgs9terr.htm
* https://www.vht-dk.dk/amiga/diverse/screenshot/terrorists.htm
* http://zine.bitfellas.org/article.php?zine=1&id=40
* http://lclevy.free.fr/uvk/uvk1.22.txt
* http://lclevy.free.fr/uvk/virus_info.txt
* http://www.memphisamigagroup.net/diskmags/199110-05/VScan5.09/VSCAN509.REA
* https://www.amigaland.com/index.php?option=com_content&view=article&id=624:amiga-computing-issue-034-1991-mar&catid=17&Itemid=122
* http://cada.probers.cz/component/phocadownload/category/10-amiga-star.html?download=139:amiga-star-10-1992&usg=AOvVaw1j5bjF80RcIZIf66htYbj_
* http://files.datassette.org/revistas/micro_sistemas_112.pdf (Page 17 "Terrorists")
* https://www.vht-dk.dk/amiga/desc/txt/novi.htm
* https://totseans.com/totse/en/viruses/virus_information/amigavir.html
* https://www.youtube.com/watch?v=kSDVQNfzbow&list=UU_66R67l9C5TUo5rEUof9uA&index=15

## Other related references
* [Die Zauberwelt der Computer-Biologie](https://www.sca.ch/amiga/virus/sca-virus-protector.html)
