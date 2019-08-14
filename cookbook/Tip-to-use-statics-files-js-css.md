# Static Files

## How let the Yesod know about your file

Yesod build the static files to use inside the project.

Example: We put one file in project:

	/static/css/my_css_file.css

The project needs to be rebuilt to bring css_my_css_file_css under the scope. If you don't build, he won't know about the file and when you use css_my_css_file_css, it won't be in the scope.
	
So to every new file added in static files you should execute the follows commands.

```
stack clean
stack build 
```

or

```
stack clean
stack build --fast --file-watch
```

The second option will automatically rebuild the code.



## How to Use Static File?

```
addStylesheet $ StaticR css_my_css_file_css
```
