Converting HTML to hamlet

```
perl -pe 'chomp' test.html | xmllint --format --encode UTF-8 - | perl -pe 's#</[^a>]+>##g'
```

This simple script may work in some simple situations but it has at least two known flaws.
* It does not remove closing tags that contain the letter 'a', such as </baz>.
* It does not handle nested lists, because xmllint sometimes prints nested lists on a single line, which the last perl in the above command does not handle well.

A much more robust solution is [meteficha's html2hamlet](https://github.com/meteficha/html2hamlet).
