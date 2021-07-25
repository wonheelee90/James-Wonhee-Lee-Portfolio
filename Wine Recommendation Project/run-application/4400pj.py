from flask import Flask
from flask import request
from flask import render_template
import pandas as pd

wine = pd.read_csv("130k.csv")
wine = wine[
    ['country', 'description', 'title', 'designation', 'points', 'price', 'province', 'region_1', 'region_2',
     'variety', 'winery']]
reviews = wine['description']
from collections import defaultdict

BoW = defaultdict(int)
for review in reviews:
    words = review.split()
    for word in words:
        BoW[word] += 1

sorted(BoW.items(), key=lambda k_v: k_v[1], reverse=True)


def filter_rating(min, max):
    rating = wine.copy()
    rating = rating.loc[rating['points'].between(min, max)]
    return rating


def filtered(seven, eight, nine, ten):
    r1 = None
    r2 = None
    r3 = None
    r4 = None

    if seven is not None:
        r1 = filter_rating(80, 85)
    if eight is not None:
        r2 = filter_rating(86, 90)
    if nine is not None:
        r3 = filter_rating(91, 95)
    if ten is not None:
        r4 = filter_rating(96, 100)
    if r1 is None and r2 is None and r3 is None and r4 is None:
        return wine
    else:
        return pd.concat([r1, r2, r3, r4])


def recommend(keywords, seven, eight, nine, ten,lower,upper):
    ff = filtered(seven, eight, nine, ten)
    wine_rec = ff
    wine_rec['recommendation'] = wine_rec['description'].str.contains(keywords[0]) * 4 + wine_rec[
                                                                                             'description'].str.contains(
        keywords[1]) * 3 + wine_rec['description'].str.contains(keywords[2]) * 2 + wine_rec['description'].str.contains(
        keywords[3]) * 1
    output=wine_rec    

    if keywords[4]!= "All":
        output = wine_rec[wine_rec["country"] == keywords[4]]

    if lower=='':
        lower=4.0
    if upper=='':
        upper=3300.0
    output=output.loc[output['price'].between(float(lower),float(upper))]
    output = output[['title', 'recommendation', 'points', 'price', 'country', 'province', 'description']].sort_values(
        by=['recommendation'], ascending=False).head(10)
    # output.reset_index(drop=True,inplace=False)
    return output


# recommend(['dry', 'cherry', 'California', 'red', 'oak'])
app = Flask(__name__)

#@app.route('/main.html')
#def Select():
	#return render_template('main.html')

@app.route('/dataset.html')
def Dataset():
	return render_template('dataset.html')

@app.route('/method.html')
def Methodology():
	return render_template('method.html')

@app.route('/diagnostics.html')
def Diagnostics():
	return render_template('diagnostics.html')

@app.route('/analysis.html')
def Analysis():
	return render_template('analysis.html')

@app.route('/', methods=['GET', 'POST'])  # allow both GET and POST requests
def form_example():
    if request.method == 'POST':  # this block is only entered when the form is submitted

        one = request.form.get('a')
        two = request.form['b']
        three = request.form.get('c')
        four = request.form['d']
        #five = request.form.get('e')
        # if one=='' or two==''or three=='' or four=='' or five=='':
        # return ("Please fill all five key words")
        six = request.form.get('comp_select')

        seven = request.form.get('rone')
        eight = request.form.get('rtwo')
        nine = request.form.get('rthree')
        ten = request.form.get('rfour')

        lower=request.form.get('lower')
        upper =request.form.get('upper')

        #if lower<4.0 or upper>3300.0 or lower is None or upper is None:
            #return ("Please enter a valid number within the range")

        result = recommend([one, two, three, four, six], seven, eight, nine, ten,lower,upper)

        return render_template('table.html', tables=[result.to_html(classes='result')], titles=['na', 'recommendation'])

    return '''<form method="POST">

<!DOCTYPE html>
<html>
<head>
<style>
* {
    box-sizing: border-box;
}

body {
    font-family: Arial;
    padding: 10px;
    background: transparent;
}

html {
  background: url(/static/big.jpg) no-repeat center center fixed;
  -webkit-background-size: cover;
  -moz-background-size: cover;
  -o-background-size: cover;
  background-size: cover;
}

/* Header/Blog Title */
.header {
    padding: 30px;
    text-align: center;

    background: url('/static/wine.jpg');
    background-size: 100% 99.9%;

}

.bag_image {
    padding: 30px;

    background: url('/static/nan.png');

    background-size: 100% 100%;;
background-repeat: no-repeat;
background-position: center;
}


.right_image_one {
    padding: 30px;

    margin-top: 20px;
        background: url('static/background.jpg');
    background-size:  100% 100%;
    background: url('/static/note1.jpg');

    background-size: 100% 100%;;
background-repeat: no-repeat;
background-position: center;
}

.right_image_two {
    padding: 30px;

    margin-top: 20px;
        background: url('/static/background.jpg');
    background-size:  100% 100%;
    background: url('/static/note2.jpg');

    background-size: 100% 100%;;
background-repeat: no-repeat;
background-position: center;
}

.right_image_three {
    padding: 30px;

    margin-top: 20px;
        background: url('static/background.jpg');
    background-size:  100% 100%;
    background: url('/static/note3.jpg');

    background-size: 100% 100%;;
background-repeat: no-repeat;
background-position: center;
}

.header h1 {
    font-size: 50px;
}

/* Style the top navigation bar */
.topnav {
    overflow: hidden;
    background-color: #333;
}

/* Style the topnav links */
.topnav a {
    float: left;
    display: block;
    color: #f2f2f2;
    text-align: center;
    padding: 14px 16px;
    text-decoration: none;
}

/* Change color on hover */
.topnav a:hover {
    background-color: #ddd;
    color: black;
}

/* Create two unequal columns that floats next to each other */
/* Left column */
.leftcolumn {
    float: left;
    width: 75%;
}

/* Right column */
.rightcolumn {
    float: right;
    width: 25%;
    background-color: #f1f1f1;
    padding-left: 20px;
}




/* Add a card effect for articles */
.card {

    padding: 20px;
    margin-top: 20px;
    background-size:  100% 100%;
}

.right_card {

    padding: 20px;
    margin-top: 20px;
        background : transparent;
    background-size:  100% 100%;
}

/* Clear floats after the columns */
.row:after {
    content: "";
    display: table;
    clear: both;
}

/* Footer */
.footer {
    padding: 5px;
    text-align: center;
    background: transparent;
    margin-top: 5px;
}

/* Responsive layout - when the screen is less than 800px wide, make the two columns stack on top of each other instead of next to each other */
@media screen and (max-width: 800px) {
    .leftcolumn, .rightcolumn {
        width: 100%;
        padding: 0;
    }
}

/* Responsive layout - when the screen is less than 400px wide, make the navigation links stack on top of each other instead of next to each other */
@media screen and (max-width: 400px) {
    .topnav a {
        float: none;
        width: 100%;
    }
}
</style>
</head>
<body>

  <div class="header">
  <h1>Wine Recommendation</h1>
</div>


<div class="topnav">
  <a href="/">Find Your Wine!</a>
  <a href="method.html">Recommendation Methodology</a>
  <a href="dataset.html">Wine Reviews Dataset</a>
  <a href="diagnostics.html">Diagnostics</a>
  <a href="analysis.html">What do experts value?</a>
  <a href="http://www.decanter.com/learn/advice/understand-tasting-notes-decoded-344920" style="float:right">Taste Note</a>
</div>

<div class="row">
  <div class="leftcolumn">

    <div id="middle" class="card"><a href="middle"></a>
<h2 align="center">What flavors describe your taste?</h2>
                      <div style="width:800px;">

                      <label>Type five flavors</label><br/>
                      Preference1: <input type="text" name="a"><br>
                      <br>
                      Preference2: <input type="text" name="b"><br>
                      <br>
                      Preference3: <input type="text" name="c"><br>
                      <br>
                      Preference4: <input type="text" name="d"><br>
                      <br>


                      <label>Select Country</label><br/>
                      <select name="comp_select">
                                        <option value="All">All</option>
                                      <option value="Italy">Italy</option>
                                      <option value="US">US</option>
                                      <option value="Portugal">Portugal</option>
                                    <option value="Spain">Spain</option>
                                      <option value="France">France</option>
                                      <option value="Germany">Portugal</option>
                                      <option value="Argentina">Argentina</option>
                                      <option value="Chile">Chile</option>
                                      <option value="Austria">Austria</option>
                                      <option value="Australia">Australia</option>
                                      <option value="South Africa">South Africa</option>
                                      <option value="New Zealand">New Zealand</option>
                                      <option value="Israel">Israel</option>
                                      <option value="Hungary">Hungary</option>
                                      <option value="Greece">Greece</option>
                                      <option value="Romania">Romania</option>
                                      <option value="Mexico">Mexico</option>
                                      <option value="Canada">Canada</option>
                                      <option value="Turkey">Turkey</option>
                                      <option value="Czech Republic">Czech Republic</option
                                      <option value="Slovenia">Slovenia</option>
                                      <option value="Luxembourg">Luxembourg</option>
                                      <option value="Croatia">Croatia</option>
                                      <option value="Georgia">Georgia</option>
                                      <option value="Uruguay">Uruguay</option>
                                      <option value="England">England</option>
                                      <option value="Lebanon">Lebanon</option>
                                      <option value="Serbia">Serbia</option>
                                      <option value="Brazil">Brazil</option>
                                      <option value="Moldova">Moldova</option>
                                      <option value="Morocco">Morocco</option>
                                      <option value="Peru">Peru</option>
                                      <option value="India">India</option>
                                      <option value="Cyprus">Cyprus</option>
                                      <option value="Bulgaria">Bulgaria</option>
                                      <option value="Armenia">Armenia</option>
                                <option value="Switzerland">Switzerland</option>
                                      <option value="Bosnia and Herzegovina">Bosnia and Herzegovina</option>
                                      <option value="Ukraine">Ukraine</option>
                              <option value="Slovakia">Slovakia</option>
                                      <option value="Macedonia">Macedonia</option>
                                      <option value="China">China</option>
                                <option value="Egypt">Egypt</option>

                                    </select><br>
                      <br>

                      Price Range (4.0,3300.0)<br/>
                      <input type="text" name="lower" size="5"> to <input type="text" name="upper" size="5"<br/>
                      <br>
                      <br>


                      Check Rating(s)<br/>
                      <input type="checkbox" name="rone" value="80-85" /> 80-85
                      <input type="checkbox" name="rtwo" value="86-90" /> 86-90
                      <input type="checkbox" name="rthree" value="91-95" /> 91-95
                      <input type="checkbox" name="rfour" value="96-100" /> 96-100  <br/>
                      <br>


                      <input type="submit" value="Submit"><br>
                      </div>

    </div>

  </div>
  <div class="rightcolumn">
    <div class="right_card">
      <h2></h2>
      <div class="right_image_one" style="height:300px;"></div>
      <p><a href="/static/note1.jpg">click to enlarge</a></p>
    </div>
    <div class="right_card">
      <h2></h2>
      <div class="right_image_three" style="height:300px;"></div>
      <p><a href="/static/note3.jpg">click to enlarge</a></p>
    </div>
    <div class="right_card">
      <h2></h2>
      <div class="right_image_two" style="height:300px;"></div>
      <p><a href="/static/note2.jpg">click to enlarge</a></p>
    </div>
  </div>
</div>

<div class="footer">
  <p>Footer</p>
      <p>Posted by: CS4400X Team1</p>
  <p>2018 Spring</p>
</div>

</body>
</html>


                  </form>'''


if __name__ == '__main__':
    app.run(debug=True)
