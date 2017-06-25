# WaveletAnalys
Sorry for my bad English. 
This project about use Wavelet Analysis in medical target. I try to use wavelets to market a vessels of the fundus. For more documentation, see my SciWork.pdf. The pdf on Russian, but has contains section 'Source' (37th page, Список использованных источников) on English. 
License of this code is MIT License.

Warning: You can't distrubute resources folder and my pdf files. Use it folder only for test this program. Use my pdf files only for understand this project.

<h2>Compiling</h2>
I used Scala IDE (current version: 4.6) and Scala (current version: 2.12.2). If you use Scala 2.11 and down, you can use '-optimise' parameter. You can use also Intellij Idea or Netbeans with Scala plugin. For build this app, use sbt (I used sbt 0.13.15).

For logs, use program arguments
	-Dlogback.configurationFile=<path/to/project>/src/main/resources/logback.xml
