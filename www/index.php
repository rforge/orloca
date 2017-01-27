<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="http://<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="http://<?php echo $themeroot; ?>/imagesrf/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<!... MMM
<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>
MMM -->

<h1>[EN] Welcome to orloca project!</h1>

<p>ORLOCA (Operations Research LOCational Analysis) deals with facility location problems. The most known is the min-sum or Fermat-Weber location problem, which seeks for a point such that the weighted sum of the distances to the demand set are minimized.</p>

<p>More information in <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>project summary page</strong></a>.</p>

<h1>[ES] ¡Bienvenido al proyecto orloca!</h1>

<p>ORLOCA (Operations Research LOCational Analysis) aborda los problemas de localización. El problema de localizaci&oacute;n m&aacute;s conocido es el problema min-sum o de Fermat-Weber, que busca un punto tal que la suma ponderada de las distancias al conjunto de demanda sea minimizada.</p>

<p>M&aacute; informaci&oacute;n en la <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>p&aacute;gina resumen del proyecto</strong></a>.</p>

</body>
</html>
