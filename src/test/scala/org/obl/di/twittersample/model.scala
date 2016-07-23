package org.obl.di.twittersample

import javax.inject.Singleton

case class OkHttpClient() {
  def sendRequest(req: String): Unit = {
    println(s"sending request $req")
  }
}

case class User(name: String)

case class TwitterApi(user: User, httpClient: OkHttpClient) {

  def sendTweet(tweet: String) = {
    val request = "[" + user + "] send tweet request : " + tweet;
    httpClient.sendRequest(request);
  }

  def getTimeline(tweetsLimit: Int): List[String] = {
    httpClient.sendRequest("[" + user + "] send get timeline request " + tweetsLimit);
    List("@rbrugier: #Dagger2 is great ! ");
  }

}

case class Tweeter(api: TwitterApi) {
  def tweet(tweetChars: String) = api.sendTweet(tweetChars)
}

case class Timeline(api:TwitterApi) {
  def getTimeline(tweetsLimit:Int):List[String] = api.getTimeline(tweetsLimit);
}

trait ApplicationComponent {
  def getTweeter():Tweeter 
  def getTimeline():Timeline 
}

trait ApplicationComponentsProvider {
  def getTweeter(user:User):Tweeter
  def getTimeline(user:User):Timeline
}

object NetworkModule {
  def provideHttpClient() = new OkHttpClient()
}

object TwitterModule {
  @Singleton
  def createTweeterApi = TwitterApi(_,_)
}

case class UserModule(user:User)

/*
object Sample {
  
  def di[I](impls:AnyRef*):I = ???
  
  val comp = di[ApplicationComponent](NetworkModule, new UserModule(User("User1")))
  
  {
    val userModule = new UserModule(User("User1"))
    val networkModule = NetworkModule
    new ApplicationComponent {
      def getTweeter():Tweeter = Tweeter(TwitterApi(userModule.user, networkModule.provideHttpClient()))
      def getTimeline():Timeline = Timeline(TwitterApi(userModule.user, networkModule.provideHttpClient()))
    }
  }

  val comp1 = di[ApplicationComponent](TwitterModule, NetworkModule, new UserModule(User("User1")))
  
  {
    val twitterModule = TwitterModule
    val userModule = new UserModule(User("User1"))
    val networkModule = NetworkModule
    val twitterApi = twitterModule.createTweeterApi(userModule.user, networkModule.provideHttpClient())
    new ApplicationComponent {
      def getTweeter():Tweeter = Tweeter(twitterApi)
      def getTimeline():Timeline = Timeline(twitterApi)
    }
  }


}
*/



