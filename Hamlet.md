Converting HTML to hamlet

```
perl -n -e 'chomp; print' | xmllint --format - | tail | perl -n -e 's/<\/[^>]+>//gi; print'
```