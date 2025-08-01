# SPDX-FileCopyrightText: 2019-2022 Pleroma Authors <https://pleroma.social>
# SPDX-License-Identifier: LGPL-2.1-only

defmodule FastHtml.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    default_pool_config = Application.get_env(:fast_html, :pool, enabled: true)
    children = if default_pool_config[:enabled], do: [FastHtml.Pool], else: []

    Supervisor.start_link(children,
      strategy: :one_for_one,
      name: FastHtml.Supervisor
    )
  end
end
