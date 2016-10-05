# movie-database-api
Movie Database Api is an application for creating and serving a movie database.

## Usage
To create an empty database with the correct schemas, run the following command, using the correct database file name:
```
$ movie-database-api create test.db
```

To serve the web api for the database, run the following command, using the correct database file name:
```
$ movie-database-api serve test.db
```

To use a specific port for the web api, you can specify the port using the `--port` flag:
```
$ movie-database-api serve test.db --port 8008
```

## License
The source code of movie-database-api is available under the [MIT license](https://opensource.org/licenses/MIT), see `LICENSE` for more information.
