
library(stargazer)

library(xtable)
library(IRdisplay)
library(repr)


display_markdown('[MD](http://commonmark.org) *formatted*')
display_html('<h1>(http://commonmark.org) *formatted*</h1>')
display_javascript('execute(this)')

display_html('<table style="text-align:center"><tr><td colspan="8" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Statistic</td><td>N</td><td>Mean</td><td>St. Dev.</td><td>Min</td><td>Pctl(25)</td><td>Pctl(75)</td><td>Max</td></tr>
	     <tr><td colspan="8" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">rating</td><td>30</td><td>64.633</td><td>12.173</td><td>40</td><td>58.8</td><td>71.8</td><td>85</td></tr>
	     <tr><td style="text-align:left">complaints</td><td>30</td><td>66.600</td><td>13.315</td><td>37</td><td>58.5</td><td>77</td><td>90</td></tr>
	     <tr><td style="text-align:left">privileges</td><td>30</td><td>53.133</td><td>12.235</td><td>30</td><td>45</td><td>62.5</td><td>83</td></tr>
	     <tr><td style="text-align:left">learning</td><td>30</td><td>56.367</td><td>11.737</td><td>34</td><td>47</td><td>66.8</td><td>75</td></tr>
	     <tr><td style="text-align:left">raises</td><td>30</td><td>64.633</td><td>10.397</td><td>43</td><td>58.2</td><td>71</td><td>88</td></tr>
	     <tr><td style="text-align:left">critical</td><td>30</td><td>74.767</td><td>9.895</td><td>49</td><td>69.2</td><td>80</td><td>92</td></tr>
	     <tr><td style="text-align:left">advance</td><td>30</td><td>42.933</td><td>10.289</td><td>25</td><td>35</td><td>47.8</td><td>72</td></tr>
	     <tr><td colspan="8" style="border-bottom: 1px solid black"></td></tr></table>')

ht <- stargazer( attitude, type='html' )
# lt <- stargazer( attitude, type='latex' )
length(ht)
o = paste(ht,sep='')
display_html(data=o) 

