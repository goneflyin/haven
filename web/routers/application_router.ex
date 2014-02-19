defmodule ApplicationRouter do
  use Dynamo.Router

  forward "/services", to: RegistrarRouter

  match "/*" do
    conn.resp 200, "Would forward to: #{conn.path_info_segments()}"
  end
end
