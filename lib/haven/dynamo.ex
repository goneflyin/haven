defmodule Haven.Dynamo do
  use Dynamo

  config :dynamo,
    # The environment this Dynamo runs on
    env: Mix.env,

    # The OTP application associated with this Dynamo
    otp_app: :haven,

    # The endpoint to dispatch requests to
    endpoint: ApplicationRouter,

    # The route from which static assets are served
    # You can turn off static assets by setting it to false
    static_route: false

  # Uncomment the lines below to enable the cookie session store
  # config :dynamo,
  #   session_store: Session.CookieStore,
  #   session_options:
  #     [ key: "_haven_session",
  #       secret: "EEufPhPvIaAnuk3ryQ4i27JQpEgmipMNLIY/fnRul7Z7XLwSyQc3SY8/LhfZyFR7"]
end
