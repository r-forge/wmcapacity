
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="/"><img src="http://r-forge.r-project.org/themes/rforge/imagesrf/logo.png" border="0" alt="R-Forge" width="205" height="54">
 </a> </td> </tr>
</table>

<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->
<hr>
<br>
<center>
<img src="images/tempbanner.jpg" border="0" alt="WoMMBAT logo" />
</center>

<!--?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

<p> WoMMBAT (Working Memory Modeling using Bayesian Analysis Techniques) is a graphical user interface for fitting Morey's (2010) hierarchical Bayesian working memory models to change detection data.</p>

<p> You can find the <strong>project summary page</strong> <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

<p><font color="red"><b>Note: due to an incompatibility in the recent version of RGtk2, if you have previously installed WoMMBAT and GTK you should uninstall GTK and reinstall from the link below. Then, reinstall the Rgtk2 and WMCapacity packages in R, either with install.packages() or update.packages().</b></font>

<H2>Installing WoMMBAT</H2>
<p>WoMMBAT is available as a package for the <a href="http://www.r-project.org/" target="null">R statistical environment</a>. R is cross-platform; it be installed and used under Windows, Mac, or Linux operating systems. However, the install process for R and WoMMBAT are slightly different for each operating system. I describe the process for each in turn. (In compiling these instructions, I was helped by the instructions for installing the R package "rattle"; if the instructions below are unclear or do not work, try installing rattle using <a href="http://www.togaware.com/datamining/survivor/Installation_Details.html" target="null">these instructions</a>, then run the WMCapacity installation code given below in R)</p>

<H3>Installing WoMMBAT under Windows</H3>

<ol>
<li> Download the latest version of R from the <a href="http://cran.r-project.org/" target="null">Comprehensive R Archive Network</a>.
<li> Download and install the latest version of the GTK+ runtime for Windows from the <a href="http://sourceforge.net/projects/gtk-win/files/GTK%2B%20Runtime%20Environment/GTK%2B%202.22/" target="null"> GTK SourceForge site</a>.
<li> Start <b>32-bit R</b>. Cut and paste the following line of code into the R console:
<p><code>
install.packages(c("gtools","coda","gWidgets","WMCapacity"),dep=TRUE)
</code> 
<p>This will install the most recent stable version of WoMMBAT inside R.
</ol>


<H3>Installing WoMMBAT under Mac OS X</H3>

<ol>
<li> Download the latest version of R from the <a href="http://cran.r-project.org/" target="null">Comprehensive R Archive Network</a>.
<li> If you haven't alread installed the Mac OS X X11 libraries, install them using your OS X install disk. Instructions may be found on the <a href="http://developer.apple.com/opensource/tools/runningx11.html" target="null">Apple developer website</a>.
<li> Download and install the latest version of the special R version of the GTK+ libraries (maintained by AT&amp;T research) from <a href="http://r.research.att.com" target="null"> here</a>. You want the GTK+ framework, under "Other binaries". If you don't have Leopard (OS X 10.5), try installing an older version of the GTK+ framework, available in their <a href="http://r.research.att.com/libs/" target="null">archive</a>.
<li> Start R. Cut and paste the following line of code into the R console:
<P><code>
install.packages(c("gtools","coda","gWidgets","WMCapacity"),dep=TRUE)
</code> 
<P>This will install the most recent stable version of WoMMBAT inside R.
</ol>


<H3>Installing WoMMBAT under Linux</H3>

<ol>
<li> Download the latest version of R from the <a href="http://cran.r-project.org/" target="null">Comprehensive R Archive Network</a>. Alternatively, you may install R from your Linux version's software repository (although it may not be the latest version).
<li> Download and install the latest version of the GTK+ libraries. They may already be installed, if you are using a GNOME desktop. Ensure you have the latest version; if not, install the latest version available through your Linux distribution's software repository.
<li> Start R. Cut and paste the following line of code into the R console:
<p><code>
install.packages(c("gtools","coda","gWidgets","WMCapacity"),dep=TRUE)
</code> 
<p>This will install the most recent stable version of WoMMBAT inside R.
</ol>

<H2>Starting the WoMMBAT Graphical User Interface</H2>

<ol>
<li>Start R in whatever way is appropriate for your operating system.
<li>Type the following code into the R console:
<code>
library(WMCapacity)
</code>
This will load the WoMMBAT package within R, and prepare it to be run.
<li>To start the GUI, type the following code into the R console:
<code>
wommbatGUI()
</code>
After running the wommbatGUI() function, a new window should open which looks like this:<br />
<img src="images/wommbatStart.png" border="0" alt="WoMMBAT start screen" width="405" height="314" />
<br />
You may now start running WoMMBAT analyses.
<li>If the interface doesn't load, send me (Richard Morey) an email at <img src="http://drsmorey.org/research/rdmorey/images/address.jpg">. Try to give me as much relevant information as possible, including any output in R when installing/loading WoMMBAT in R, what operating system you are running, and what version of R you are running. I'll try to get back to you as soon as possible. 
</ol>



</body>
</html>
