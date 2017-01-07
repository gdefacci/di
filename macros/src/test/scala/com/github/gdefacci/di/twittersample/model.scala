package com.github.gdefacci.di.twittersample

import com.github.gdefacci.di.runtime.Bind
import com.github.gdefacci.di.runtime.ApplicationScope

class OkHttpClient() {
  def sendRequest(req: String): Unit = {
    println(s"sending request $req")
  }
}

case class User(name: String)

case class TwitterApi(user: User, httpClient: OkHttpClient) {

  def sendTweet(tweet: String) = {
    val request = "[" + user + "] send tweet request : " + tweet
    httpClient.sendRequest(request)
  }

  def getTimeline(tweetsLimit: Int): List[String] = {
    httpClient.sendRequest("[" + user + "] send get timeline request " + tweetsLimit)
    List("@rbrugier: #Dagger2 is great ! ")
  }

}

case class Tweeter(api: TwitterApi) {
  def tweet(tweetChars: String) = api.sendTweet(tweetChars)
}

case class Timeline(api:TwitterApi) {
  def getTimeline(tweetsLimit:Int):List[String] = api.getTimeline(tweetsLimit)
}

trait ApplicationComponent {
  def getTweeter():Tweeter 
  def getTimeline():Timeline 
}

case class ApplicationComponentImpl(getTweeter:Tweeter, getTimeline:Timeline) 

trait ApplicationComponentsProvider {
  def getTweeter(user:User):Tweeter
  def getTimeline(user:User):Timeline
}

object NetworkModule {
  def provideHttpClient() = new OkHttpClient()
}

object TwitterModule {
  
  @ApplicationScope val bindHttpClient = Bind.bind[OkHttpClient]
  @ApplicationScope def createTweeterApi(user: User, httpClient: OkHttpClient) = TwitterApi(user, httpClient)
}

case class TweeterService1(f:User => Tweeter, client:OkHttpClient)

object TwitterModule1 {
  
  @ApplicationScope val bindHttpClient = Bind.bind[OkHttpClient]
  @ApplicationScope def createTweeterApi(user: User, httpClient: OkHttpClient) = TwitterApi(user, httpClient)
  
  @ApplicationScope val fbind = Bind.bind[User => Tweeter]
  
}


case class UserModule(user:User)



