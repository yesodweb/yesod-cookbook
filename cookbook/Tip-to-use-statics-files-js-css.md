# Static Files

## How let the Yesod know about your file

Yesod build the static files to use inside the project.

Exemple We have one file in project:

	/static/css/my_css_file.css

When Yesod build the libraries, a variable with name css_my_css_file_css is created to use inside Handlers.

If you don't build, he no know about the file and when you use css_my_css_file_css, it no exists.

	
So, every new file add in static file you should execute the follows commands

```
stack clean
stack build
```

## How Use Static File?

```
addStylesheet $ StaticR css_my_css_file_css
```