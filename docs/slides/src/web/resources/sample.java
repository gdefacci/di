class Service {
  @Inject public Service(Repository repo) {
    this.repository = repo;
  }
}

class Repository {
  @Inject public Repository(Connection connection) {
    this.connection = connection;
  }
}

class Connection {
  @Inject public Connection(@Named("default") User user) {
    this.user = user;
  }
}