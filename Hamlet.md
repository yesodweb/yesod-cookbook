Converting HTML to hamlet

```
perl -pe 'chomp' test.html | xmllint --format --encode UTF-8 - | perl -pe 's#</[^>]+>##g'
```