## Autocrop

Autocrop is a way of removing "excess" background on an image. Let's say you had an image that was surrounded by a lot of white background, and you wanted to remove that white so that all that remained was the image proper, then autocrop is what you want.

It works by checking the pixels of each row and column at the sides, top, and bottom, and working inwards. It removes those rows and columns if they consist entirely of the color that we wish to remove.

Very simple to use, simply invoke on an image and pass in a color that should be used as the background detection color. 

For example to remove excess white background:

```
image.autocrop(Color.White)
```
or to remove a background of a custom color you could do:
```
image.autocrop(new Color(255,235,10))
```

As always a picture is worth a thousand bytecodes. It's hard to see the white background in this example so I've added a 1 pixel black border around the output so its clearer to see how the white space was removed.

<table>
<tr>
<th>
    Before
</th>
<th>
    After
</th>
</tr>
<tr>
<td>
    <img src="https://raw.github.com/sksamuel/scrimage/master/examples/images/dyson.png"/>
</td>
<td>
    <img src="https://raw.github.com/sksamuel/scrimage/master/examples/images/dyson_autocropped.png"/>
</td>
</tr>
</table>
